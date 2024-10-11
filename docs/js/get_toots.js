export async function getToots(account_id, max_id) {
  const max_id_str = max_id ? "&max_id=" + max_id : "";
  const url = "https://ecoevo.social/api/v1/accounts/" + account_id +
    "/statuses?exclude_replies=true&exclude_reblogs=true" + max_id_str;
  console.log("Sending request: " + url);
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Response status: ${response.status}`);
    }

    const json = await response.json();
    return json.filter((row) => {return !row.in_reply_to_id});;
  } catch (error) {
    console.error(error.message);
  }
}

// export async function getToots(account_id, nr_toots) {
//   async function worker(toot_list) {
//     if(toot_list.length >= nr_toots) {
//       return toot_list;
//     } else {
//       const max_id_str = toot_list.length == 0 ? "" : "&max_id=" + toot_list[toot_list.length - 1].id;
//       const url = "https://ecoevo.social/api/v1/accounts/" + account_id +
//         "/statuses?exclude_replies=true&exclude_reblogs=true" + max_id_str;
//       console.log("Sending request: " + url);
//       const new_toots = await d3.json(url);
//       const new_toots_filtered = new_toots.filter((row) => {return row.in_reply_to_id});
//       console.log("Adding " + new_toots_filtered.length + " toots");
//       const res = await worker(toot_list.concat(new_toots_filtered));
//       return res;
//     }
//   }
//   return worker([]);
// }