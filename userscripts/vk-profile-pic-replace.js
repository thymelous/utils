// ==UserScript==
// @name         VK Profile Pic Replace
// @version      0.3
// @description  Replace your friends' profile pics in one chat without affecting
//               the rest of your conversations
// @author       timlathy
// @match        https://vk.com/im*
// @grant        none
// ==/UserScript==

(function () {
  'use strict';

  // Settings
  const perChatProfileReplacements = {
    'chat name as displayed in your conversation list': {
      // vk.com/user_short_name
      'user_short_name': { pic: 'https://i.imgur.com/replacement-url-here.png', name: 'Joe' },
      // vk.com/id1000000000000
      'id1000000000000': { pic: 'https://i.imgur.com/another-url-here.png', name: 'h' }
    }
  };

  const perChatTypingReplacements = {
    'chat name as displayed in your conversation list': [
      { old: 'Full Nameovich', new: 'Joe' }
    ]
  };

  const chatName = (node) => node?.textContent?.trim().replace(/\s/g, ' ');
  let currentChat = chatName(document.querySelector('.im-page--title-main'));
  let currentProfileReplacements = perChatProfileReplacements[currentChat];
  let currentTypingReplacements = perChatTypingReplacements[currentChat];

  function replaceProfiles(message) {
    const profileNames = message.querySelectorAll('.im-mess-stack--lnk');
    for (let name of profileNames) {
      const user = name.getAttribute('href').substr(1);
      const replacement = currentProfileReplacements[user];
      replacement && (name.textContent = replacement.name);
    }

    const profilePics = message.querySelectorAll('.im-mess-stack--photo .im_grid');
    for (let pic of profilePics) {
      const user = pic.getAttribute('href').substr(1);
      const replacement = currentProfileReplacements[user];
      replacement && (pic.firstElementChild.src = replacement.pic);
    }
  }

  function replaceTypingInfo(typingInfo, replacements) {
    const names = typingInfo.textContent
      .replace(/ (is|are) typing/, '')
      .split(/\s*and\s*/)
      .map((n) => replacements.find(({ old }) => old.startsWith(n.replace('.', '')))?.new || n);
    typingInfo.textContent = names.join(' and ') + (names.length === 1 ? ' is ' : ' are ') + 'typing';
  }

  function replaceSideBarPreview(previewText, container) {
    const chat = chatName(container.closest('.nim-dialog')?.querySelector('._im_dialog_link'));
    const replacements = perChatTypingReplacements[chat];
    if (!replacements)
      return;
    if (!previewText.textContent.endsWith(':'))
      return replaceTypingInfo(previewText, replacements);

    const previewName = previewText.textContent.slice(0, -1);
    const replacement = replacements.find(({ old }) => old.startsWith(previewName))?.new;
    replacement && (previewText.textContent = replacement + ':');
  }

  // Replace profile pics in the currently open chat
  currentProfileReplacements && document.querySelectorAll('.im-mess-stack').forEach(replaceProfiles);

  // Observe page modifications
  new MutationObserver(onPageChange).observe(document.getElementById('page_body'), { childList: true, subtree: true });

  let chatsOpen = false;
  function onPageChange(records) {
    if (!window.location.href.startsWith('https://vk.com/im')) {
      chatsOpen = false;
      return;
    }
    // Just opened the chats page, refresh the side bar
    if (!chatsOpen) {
      chatsOpen = true;
      document.querySelectorAll('.nim-dialog--who').forEach((d) => replaceSideBarPreview(d.childNodes[0], d));
    }
    for (let record of records) {
      // Opening a chat
      if (record.target.classList.contains('im-page--title-wrapper') && record.addedNodes.length > 0) {
        currentChat = record.addedNodes[0].firstElementChild?.title;
        currentProfileReplacements = perChatProfileReplacements[currentChat];
        currentTypingReplacements = perChatTypingReplacements[currentChat];
      }
      // Loading new messages
      else if (currentProfileReplacements && record.target.classList.contains('im-page-chat-contain'))
        record.addedNodes.forEach(replaceProfiles);
      // Updating "X is typing..." status message
      else if (currentTypingReplacements && record.target.classList.contains('_im_typing_name') && record.addedNodes.length > 0)
        replaceTypingInfo(record.addedNodes[0], currentTypingReplacements);
      // Updating latest message preview in the side bar
      // Note: this occurs regardless of what chat is open
      else if (record.target.classList.contains('nim-dialog--preview') && record.addedNodes.length > 0)
        replaceSideBarPreview(record.addedNodes[0], record.target);
      // Updating "X is typing..." in the side bar
      // Note: this occurs regardless of what chat is open
      else if (record.target.classList.contains('nim-dialog--typing') && record.addedNodes.length > 0)
        replaceSideBarPreview(record.addedNodes[0], record.target);
    }
  }
})();
