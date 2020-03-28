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

  function replaceProfilePic(messageNode) {
    const pics = messageNode.querySelectorAll('.im-mess-stack--photo .im_grid');
    for (let pic of pics) {
      const user = pic.getAttribute('href').substr(1);
      const replacement = currentProfilePicReplacements[user];
      if (replacement)
        pic.children[0].src = replacement;
    }
  }

  // Replace profile pics in the currently open chat
  const currentChat = document.querySelector('.im-page--title-main').title;
  let currentProfilePicReplacements = perChatProfilePicReplacements[currentChat];
  document.querySelectorAll('.im-mess-stack').forEach(replaceProfilePic);

  // Observe switching between chats
  new MutationObserver((records) => {
    for (let record of records) {
      if (record.addedNodes.length == 1 && record.addedNodes[0].firstElementChild) {
        const currentChat = record.addedNodes[0].firstElementChild.title;
        currentProfilePicReplacements = perChatProfilePicReplacements[currentChat];
      }
    }
  }).observe(document.querySelector('.im-page--title-wrapper'), { childList: true });

  // Observe loading new messages
  new MutationObserver((records) => {
    if (currentProfilePicReplacements) {
      for (let record of records) {
        record.addedNodes.forEach(replaceProfilePic);
      }
    }
  }).observe(document.querySelector('.im-page-chat-contain'), { childList: true });

})();
