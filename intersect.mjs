import * as fs from "node:fs";

const old = JSON.parse(fs.readFileSync("json/on_prem_702.json"));
const steam = JSON.parse(fs.readFileSync("json/steampunk_2111.json"));

for (const oldObj of old.object_list) {
  if (oldObj.exists === false) {
    continue;
  }

  for (const newObj of steam.object_list) {
    if (newObj.exists === false) {
      continue;
    }

    if (oldObj.object === newObj.object && oldObj.obj_name === newObj.obj_name) {
      console.dir(oldObj.obj_name);

      let base = `${oldObj.obj_name.toLowerCase()}.${oldObj.object.toLowerCase()}.xml`;
      let filename = `../steampunk-2305-api/src/` + base;
      if (fs.existsSync(filename)) {
        fs.copyFileSync(filename, "./intersect/" + base);
      }

      base = `${oldObj.obj_name.toLowerCase()}.${oldObj.object.toLowerCase()}.abap`;
      filename = `../steampunk-2305-api/src/` + base;
      if (fs.existsSync(filename)) {
        fs.copyFileSync(filename, "./intersect/" + base);
      }
    }
  }
}