import * as DB from "../db";
import { dataTypes } from "../lazy-fm";

export class InputStandards extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    this.innerHTML = initialHtml;

    this.addEventListener("input", this.saveValues);

    this.readValues();
  }

  disconnectedCallback() {}

  attributeChangedCallback(name: string, oldValue: string, newValue: string) {
    console.log(`Attribute has changed.`, name, oldValue, newValue);
  }

  saveValues() {
    const inputs = this.querySelectorAll("input");
    const data: any = {};

    for (const input of Array.from(inputs)) {
      const posGroup = input.getAttribute("data-position-group") as string;
      const attr = input.getAttribute("data-attr") as string;
      const val = parseFloat(input.value);

      if (posGroup && !isNaN(val)) {
        if (!data[posGroup]) data[posGroup] = {};
        if (!data[posGroup][attr]) data[posGroup][attr] = {};
        data[posGroup][attr] = val;
      }
    }

    DB.saveStandards(data);
  }

  readValues() {
    const standards = DB.readStandards();
    for (const [posg, attrs] of Object.entries(standards)) {
      for (const [attr, v] of Object.entries(attrs)) {
        const std = "avg";
        const elem = this.querySelector(
          `input[data-position-group="${posg}"][data-attr="${attr}"][data-type="${std}"]`,
        );
        if (elem instanceof HTMLInputElement) {
          elem.value = String(v);
        } else {
          console.error(`Can't find input for`, posg, attr, std);
        }
      }
    }
  }
}

customElements.define("fm-input-standards", InputStandards);

const attrNames = Object.fromEntries(dataTypes.attrs);
const sgNames = Object.fromEntries(dataTypes.standardGroupNames);

const attrRowsHtml = dataTypes.standardGroupAttrs
  .map(([pg, attrs]) => {
    const attrRow = (attr: string) => `<tr>
      <td>${attrNames[attr]}</td>
      <td>${attr}</td>
      <td><input data-position-group="${pg}" data-attr="${attr}" data-type="avg"></td>
    </tr>
    `;

    return `<tr><th colspan=4>${sgNames[pg]}</th></tr>
      ${attrs.map(attrRow).join("\n")}
     </tr>`;
  })
  .join("\n");

const initialHtml = `<form>
    <table>
        <tr>
          <th>Name</th>
          <th>Code</th>
          <th>Avg</th>
        </tr>
        ${attrRowsHtml}
      </table>
    </form>`;
