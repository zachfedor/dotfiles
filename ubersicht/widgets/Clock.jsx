/**
 * CLOCK
 * An Übersicht Widget
 */
import { css } from "uebersicht";

const command = "whoami";

// Refresh every second
const refreshFrequency = 1000;

// Styles
const className = `
  color: rgba(253, 245, 224, 0.9);
  font-family: EtBembo, Merriweather, Georgia, serif;
  font-smoothing: antialiased;
  font-weight: 300;
  height: 300px;
  left: 40px;
  text-shadow: 0px 0px 20px rgba(0, 0, 0, 0.6);
  top: 210px;
  width: 800px;
`;
// font-family: Source Sans Pro, Helvetica Neue, sans-serif;
const time = css`
  font-size: 8rem;
`;
const hour = css``;
const min = css``;
const sep = css`
  font-size: 6rem;
`;
const text = css`
  font-size: 3rem;
  font-style: normal;
  text-shadow: 0px 0px 20px rgba(0, 0, 0, 0.9);
`;

// Render the component
const render = ({ output, error }) => {
  const now = new Date();
  const hourStr = now.getHours().toString();
  const minStr = now.getMinutes().toString().padStart(2, "0");

  return (
    <div>
      <div className={time}>
        <span className={hour}>{hourStr}</span>
        <span className={sep}>:</span>
        <span className={min}>{minStr}</span>
      </div>

      <div className={text}>Simplicitas. Diligentia. Ars.</div>
    </div>
  );
};

export { className, command, refreshFrequency, render };
