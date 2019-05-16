export const getNewsItem = ({ title, subtitle, text }) => {
	return `
    <table mc:repeatable="content" mc:variant="Tekstblok met introtekst" width="100%" cellpadding="0" cellspacing="0" border="0">
      <tr>
        <td height="30" style="height:30px;line-height:0;">

         </td>
      </tr>
      <tr>
        <td style="padding:5px 0 15px 0;">
          <font style="color:#333332;font-family:Calibri, Arial, sans-serif;font-size:26px;font-weight:600;line-height:26px;">${title}</font>
          <p class="intro-text" style="color:#666666;font-family:Calibri, Arial, sans-serif;font-size:15px;line-height:20px;margin-top:5px;margin-bottom:0;">
            ${subtitle}
          </p>
        </td>
      </tr>
      <tr>
        <td style="color:#666666;font-family:Calibri, Arial, sans-serif;font-size:17px;line-height:26px;">
          <p>${text}</p>
        </td>
      </tr>
    </table>
    
  `
};
