import * as DB from "../db";

const initialHtml = `<form>
  <input type="file">
</form>`;

function parseTable(rawText: string): { [k: string]: string }[] {
  const text = rawText.replaceAll("<B>", "").replaceAll("</B>", "");

  const doc = new DOMParser().parseFromString(text, "text/html");
  const rows = Array.from(doc.querySelectorAll("tr"));

  const headerRow = rows[0];
  const playerRows = rows.slice(1);

  const cols = Array.from(headerRow.querySelectorAll("th")).map((el) =>
    el.innerText.trim(),
  );

  return playerRows.map((el) =>
    Array.from(el.querySelectorAll("td")).reduce(
      (p, cell, idx) => {
        p[cols[idx]] = cell.innerText.trim();
        return p;
      },
      {} as { [k: string]: string },
    ),
  );
}

export class PlayerImport extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    this.innerHTML = initialHtml;

    this.addEventListener("input", this.readAndSave);
  }

  disconnectedCallback() {}

  async readAndSave() {
    const elem = this.querySelector('input[type="file"]') as HTMLInputElement;

    const files = elem.files;
    if (files == null || files.length != 1) {
      alert("Select exactly one file");
      return;
    }

    const table = parseTable(await files[0].text());
    DB.savePlayers(table);
  }
}

customElements.define("fm-player-import", PlayerImport);
