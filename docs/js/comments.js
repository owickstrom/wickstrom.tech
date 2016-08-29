function initializeComments(pageUrl, identifier) {
  var commentsLink = document.getElementById('post-comments-link');
  var postComments = document.getElementById('post-comments');

  function showComments() {
    commentsLink.className = 'hidden';
    postComments.className = 'post-comments';
  }

  function loadDisqus() {
    window.disqus_config = function () {
      this.page.url = pageUrl;
      this.page.identifier = identifier;
    };
    (function() { // DON'T EDIT BELOW THIS LINE
      var d = document, s = d.createElement('script');

      s.src = '//owickstrom.disqus.com/embed.js';

      s.setAttribute('data-timestamp', +new Date());
      postComments.querySelector('.wrapper').appendChild(s);
    })();
  }

  commentsLink.addEventListener('click', showComments);

  if (/\#(disqus_thread|comment-)/.test(window.location.hash)) {
    showComments();
  }

  loadDisqus();
}
