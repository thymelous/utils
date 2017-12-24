// ==UserScript==
// @name         Pinterest Source Search
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  Makes it easier to find sources of pins uploaded from external sites.
// @author       thymelous
// @match        https://*.pinterest.com/*
// @grant        GM_xmlhttpRequest
// ==/UserScript==

/* Changelog:
 * 
 * Dec 24, 2017
 *   Initial version
 */

(function() {
  'use strict';

  /* ------
   * The script adds a "Source Search" mode for Pinterest grids,
   * which replaces the Save button you get by hovering over a pin
   * with Find Source.
   *
   * When the button is pressed, */
  function findImage(e) {
    e.stopPropagation();
    const pin = e.target.closest('.pinWrapper');

    /* We grab the pin's source URL */
    let url = pin.querySelector('a.navigateLink') && pin.querySelector('a.navigateLink').href;

    /* See if it's originally a KYM submission, and if it is,
     * try to fish out the source from there, */
    if (url && /knowyourmeme.com\/photos\/\d+/.test(url)) openKymSource(pin, url);
    else openSource(pin, url);
  }

  /* To go the source URL, we: */
  function openSource(pin, url) {
    /* Check if it points to a Tumblr submission
     * (not a search page or a blog home page!), and if it does: */
    if (url && /\w+.tumblr.com\/(post|image)\/\d+/.test(url)) {
      /* Replace /image/, which returns a raw image,
       * with /post/, which returns the whole submission with artist notes and so on */
      url = url.replace('tumblr.com/image', 'tumblr.com/post');
      /* Open it in a new tab. */
      window.open(url);
    }
    /* Check if it points to a DeviantArt submission
     * (not an artist's page or search results!), and if it does: */
    else if (url && /\w+.tumblr.com\/(post|image)\/\d+/.test(url)) {
      /* Open it in a new tab. */
      window.open(url);
    }
    /* Otherwise, query Google Image search: */
    else {
      /* Grab the thumbnail */
      const thumbUrl = pin.querySelector('img').src;
      /* Exclude Pinterest from search results */
      const query = '-site:pinterest.com';
      /* Open the search page in a new tab */
      const searchUrl = `https://www.google.com/searchbyimage?&image_url=${thumbUrl}&q=${query}`;
      window.open(searchUrl);
    }
  }

  /* To get the source from a KYM submission, we: */
  function openKymSource(pin, url) {
    /* First, download the page */
    GM_xmlhttpRequest({
      method: "GET", url: url,
      onload: function(response) {
        /* Then put it into an element we can query */
        const kymPage = document.createElement('html');
        kymPage.innerHTML = response.responseText;

        /* And grab the URL if it's present */
        const sourceLink = kymPage.querySelector('.media-notes a.external-link');
        const kymUrl = sourceLink && sourceLink.href;

        /* If it is not, we just open a Google Image search page */
        openSource(pin, kymUrl || url);
      }
    });
  }

  /* Enable Source Search: */
  function observePinHover() {
    /* Observe changes to the grid: */
    new MutationObserver((records) => {
      /* When Pinterest's code inserts the Save button, */
      const buttonChange = records.find((r) => r.target.classList.contains('pinImageActionButtonWrapper'));
      const buttons = buttonChange && buttonChange.addedNodes[0];

      if (buttons) {
        /* Replace it with "Find Source" */
        buttons.querySelector('.SaveButton div div div:last-child').textContent = 'Find Source';
        buttons.querySelector('.leftSideButtonsWrapper').addEventListener('click', findImage);
      }
    }).observe(document.querySelector('.BoardPinGrid'), { childList: true, subtree: true });
  }

  /* Inject the Source Search button: first, pick an existing button, */
  const button = document.querySelector('.infoBar .pull-right button');
  button && insertScriptToggle(button);

  function insertScriptToggle(button) {
    /* Then clone and adjust a bit */
    const scriptToggle = button.cloneNode(true);

    scriptToggle.classList.add('pinterest-image-search-script-toggle');
    scriptToggle.style.marginLeft = '10px';
    scriptToggle.childNodes[0].textContent = 'Source Search';
    scriptToggle.addEventListener('click', (e) => {
      e.target.closest('.pinterest-image-search-script-toggle').remove();
      observePinHover();
    });

    button.parentNode.appendChild(scriptToggle);
  }

})();
