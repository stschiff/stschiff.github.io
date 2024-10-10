---
title: News
menu_News: True
---

<div id="toot_list"></div>

<script type="module">
    import { getToots } from "/js/get_toots.js";
    const toots = await getToots("109301761847534867");
    console.log(toots);
    const toot_list = document.getElementById('toot_list');
    for(const toot of toots) {
        const toot_div = document.createElement('article')
        let toot_img = "";
        if(toot.media_attachments.length > 0) {
          toot_img = `<img src="` + toot.media_attachments[0].preview_url + `" />`;
        }
        const toot_time = new Date(toot.created_at);
        const now = new Date();
        const diff_ms = now.getTime() - toot_time.getTime();
        let time_string = "";
        if(diff_ms < 60 * 60 * 1000) {
          time_string = Math.round(diff_ms / (60 * 1000)) + "m ago";
        }
        else if(diff_ms < 24 * 60 * 60 * 1000) {
          time_string = Math.round(diff_ms / (60 * 60 * 1000)) + "h ago";
        }
        else if(diff_ms < 7 * 24 * 60 * 60 * 1000) {
          time_string = Math.round(diff_ms / (24 * 60 * 60 * 1000)) + "d ago";
        }
        else {
          time_string = toot_time.toDateString();
        }
        toot_div.className = "media";
        toot_div.innerHTML = `
  <figure class="media-left">
    <p class="image is-64x64">
      <a href="` + toot.account.url + `"><img src="` + toot.account.avatar + `" /></a>
    </p>
  </figure>
  <div class="media-content">
    <div class="content">
      <div class="columns">
        <div class="column is-two-thirds">
          <p>
            <a href="` + toot.account.url + `"><strong>`+ toot.account.display_name + `</strong>
            <small>@` + toot.account.username + `@ecoevo.social</small></a>
            <small>` + time_string + `</small>
            <br />
` + toot.content + `        
          </p>
          <a href="` + toot.url + `"><small><i class="fa-solid fa-arrow-up-right-from-square"></i> Jump to Post</small></a>
        </div>
        <div class="column">` + toot_img + `</div>
      </div>
      
    </div>
  </div>`
        toot_list.appendChild(toot_div);
    }
</script>