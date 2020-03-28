// ==UserScript==
// @name         VK Profile Pic Replace
// @version      0.1
// @description  Replace your friends' profile pics in one chat without affecting
//               the rest of your conversations
// @author       timlathy
// @match        https://vk.com/im*
// @grant        none
// ==/UserScript==

(function() {
  'use strict';

  // Settings
  const perChatProfilePicReplacements = {
    'chat name as displayed in your conversation list': {
      // vk.com/user_short_name
      'user_short_name': 'https://i.imgur.com/replacement-url-here.png',
      // vk.com/id1000000000000
      'id1000000000000': 'https://i.imgur.com/another-url-here.png',
    }
  };

  let currentReplacements;

  function replaceProfilePic(message) {
    const profilePics = message.querySelectorAll('.im-mess-stack--photo .im_grid');
    for (let pic of profilePics) {
      const user = pic.getAttribute('href').substr(1);
      const replacement = currentReplacements[user];
      if (replacement)
        pic.firstElementChild.src = replacement;
    }
  }

  // Replace profile pics in the currently open chat
  const currentChat = document.querySelector('.im-page--title-main');
  currentReplacements = currentChat && perChatProfilePicReplacements[currentChat.title];
  currentReplacements && document.querySelectorAll('.im-mess-stack').forEach(replaceProfilePic);

  // Observe page modifications
  new MutationObserver((records) => {
    // Closing a chat and switching to a different page (news feed, communities, etc.)
    if (!window.location.href.startsWith('https://vk.com/im?sel'))
      return;
    for (let record of records) {
      // Opening a chat
      if (record.target.classList.contains('im-page--title-wrapper') && record.addedNodes.length == 1) {
        const currentChat = record.addedNodes[0].firstElementChild;
        if (currentChat)
          currentReplacements = perChatProfilePicReplacements[currentChat.title];
      }
      // Loading new messages
      if (currentReplacements && record.target.classList.contains('im-page-chat-contain'))
        record.addedNodes.forEach(replaceProfilePic);
    }
  }).observe(document.getElementById('page_body'), { childList: true, subtree: true });
})();
