package org.openjump.ext.setattributes;

import com.vividsolutions.jump.I18N;

import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;

/**
 * Creates a tooltip explaining what the button does.
 */
public class SetAttributesButtonMouseListener extends MouseAdapter {

    final I18N I18N_ = I18N.getInstance("set_attributes");
    final int defaultDismissTimeout = ToolTipManager.sharedInstance().getDismissDelay();
    final int defaultInitialDelay = ToolTipManager.sharedInstance().getInitialDelay();

    final SetOfAttributes setOfAttributes;
    final JComponent component;

    protected SetAttributesButtonMouseListener(final SetOfAttributes setOfAttributes,
                                               final JComponent component) {
        this.setOfAttributes = setOfAttributes;
        this.component = component;
    }

    @Override
    public void mousePressed(MouseEvent e) {
        if (SwingUtilities.isRightMouseButton(e)) {
            ToolTipManager.sharedInstance().setDismissDelay(10000);
            ToolTipManager.sharedInstance().setInitialDelay(100);
            component.setToolTipText(getHtmlToolTip(setOfAttributes));
            MouseEvent phantom = new MouseEvent(
                    component,
                    MouseEvent.MOUSE_PRESSED,
                    System.currentTimeMillis(),
                    0,
                    0,
                    0,
                    0,
                    false);
            ToolTipManager.sharedInstance().mouseMoved(phantom);
        }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
        if (SwingUtilities.isRightMouseButton(e)) {
            component.setToolTipText(setOfAttributes.getTooltip());
            ToolTipManager.sharedInstance().setDismissDelay(defaultDismissTimeout);
            ToolTipManager.sharedInstance().setInitialDelay(defaultInitialDelay);
        }
    }

    public String getHtmlToolTip(SetOfAttributes button) {
        StringBuilder text = new StringBuilder("<html>");
        text.append("<b>");
        text.append(I18N_.getText("set_attributes", "SetAttributesPlugIn.layer"));
        text.append("</b> : ");
        text.append(forHTML(button.getLayer()));

        text.append("<br><b>");
        text.append(I18N_.getText("set_attributes", "SetAttributesPlugIn.transaction-mode"));
        text.append("</b> : ");
        text.append(button.isAtomic() ?
        I18N_.getText("set_attributes", "SetAttributesPlugIn.transaction-mode.atomic") :
        I18N_.getText("set_attributes", "SetAttributesPlugIn.transaction-mode.normal"));

        if (button.getDimension() > -1) {
        text.append("<br><b>");
        text.append(I18N_.getText("set_attributes", "SetAttributesPlugIn.accepted-geometry-dimension"));
        text.append("</b> : ");
        text.append(button.getDimension());
        }

        text.append("<br><b>");
        text.append(I18N_.getText("set_attributes", "SetAttributesPlugIn.attributes-changed"));
        text.append("</b> :");
        text.append("<ul>");
        for (SetAttribute att : button.getAttributes()) {
        text.append("<li><b>").append(att.getName()).append("</b> : ").append(att.getValue());
        if (att.getPrerequisite() != null && att.getPrerequisite().length() > 0) {
        text.append("<i>[");
        text.append(I18N_.getText("set_attributes", "SetAttributesPlugIn.attributes-changed.if"));
        text.append(" ");
        text.append(forHTML(att.getPrerequisite()));
        text.append("]</i>");
        }
        text.append("</li>");
        }
        text.append("</ul></html>");
        return text.toString();
        }

    /**
     Escape characters for text appearing in HTML markup.

     <P>This method exists as a defence against Cross Site Scripting (XSS) hacks.
     The idea is to neutralize control characters commonly used by scripts, such that
     they will not be executed by the browser. This is done by replacing the control
     characters with their escaped equivalents.

     <P>The following characters are replaced with corresponding
     HTML character entities :
     <table border='1' cellpadding='3' cellspacing='0'>
     <tr><th> Character </th><th>Replacement</th></tr>
     <tr><td> < </td><td> &lt; </td></tr>
     <tr><td> > </td><td> &gt; </td></tr>
     <tr><td> & </td><td> &amp; </td></tr>
     <tr><td> " </td><td> &quot;</td></tr>
     <tr><td> \t </td><td> &#009;</td></tr>
     <tr><td> ! </td><td> &#033;</td></tr>
     <tr><td> # </td><td> &#035;</td></tr>
     <tr><td> $ </td><td> &#036;</td></tr>
     <tr><td> % </td><td> &#037;</td></tr>
     <tr><td> ' </td><td> &#039;</td></tr>
     <tr><td> ( </td><td> &#040;</td></tr>
     <tr><td> ) </td><td> &#041;</td></tr>
     <tr><td> * </td><td> &#042;</td></tr>
     <tr><td> + </td><td> &#043; </td></tr>
     <tr><td> , </td><td> &#044; </td></tr>
     <tr><td> - </td><td> &#045; </td></tr>
     <tr><td> . </td><td> &#046; </td></tr>
     <tr><td> / </td><td> &#047; </td></tr>
     <tr><td> : </td><td> &#058;</td></tr>
     <tr><td> ; </td><td> &#059;</td></tr>
     <tr><td> = </td><td> &#061;</td></tr>
     <tr><td> ? </td><td> &#063;</td></tr>
     <tr><td> @ </td><td> &#064;</td></tr>
     <tr><td> [ </td><td> &#091;</td></tr>
     <tr><td> \ </td><td> &#092;</td></tr>
     <tr><td> ] </td><td> &#093;</td></tr>
     <tr><td> ^ </td><td> &#094;</td></tr>
     <tr><td> _ </td><td> &#095;</td></tr>
     <tr><td> ` </td><td> &#096;</td></tr>
     <tr><td> { </td><td> &#123;</td></tr>
     <tr><td> | </td><td> &#124;</td></tr>
     <tr><td> } </td><td> &#125;</td></tr>
     <tr><td> ~ </td><td> &#126;</td></tr>
     </table>
     */
    public static String forHTML(String aText){
    final StringBuilder result = new StringBuilder();
    final StringCharacterIterator iterator = new StringCharacterIterator(aText);
        char character =  iterator.current();
        while (character != CharacterIterator.DONE ){
        if (character == '<') {
        result.append("&lt;");
        }
        else if (character == '>') {
        result.append("&gt;");
        }
        else if (character == '&') {
        result.append("&amp;");
        }
        else if (character == '\"') {
        result.append("&quot;");
        }
        else if (character == '\t') {
        addCharEntity(9, result);
        }
        else if (character == '!') {
        addCharEntity(33, result);
        }
        else if (character == '#') {
        addCharEntity(35, result);
        }
        else if (character == '$') {
        addCharEntity(36, result);
        }
        else if (character == '%') {
        addCharEntity(37, result);
        }
        else if (character == '\'') {
        addCharEntity(39, result);
        }
        else if (character == '(') {
        addCharEntity(40, result);
        }
        else if (character == ')') {
        addCharEntity(41, result);
        }
        else if (character == '*') {
        addCharEntity(42, result);
        }
        else if (character == '+') {
        addCharEntity(43, result);
        }
        else if (character == ',') {
        addCharEntity(44, result);
        }
        else if (character == '-') {
        addCharEntity(45, result);
        }
        else if (character == '.') {
        addCharEntity(46, result);
        }
        else if (character == '/') {
        addCharEntity(47, result);
        }
        else if (character == ':') {
        addCharEntity(58, result);
        }
        else if (character == ';') {
        addCharEntity(59, result);
        }
        else if (character == '=') {
        addCharEntity(61, result);
        }
        else if (character == '?') {
        addCharEntity(63, result);
        }
        else if (character == '@') {
        addCharEntity(64, result);
        }
        else if (character == '[') {
        addCharEntity(91, result);
        }
        else if (character == '\\') {
        addCharEntity(92, result);
        }
        else if (character == ']') {
        addCharEntity(93, result);
        }
        else if (character == '^') {
        addCharEntity(94, result);
        }
        else if (character == '_') {
        addCharEntity(95, result);
        }
        else if (character == '`') {
        addCharEntity(96, result);
        }
        else if (character == '{') {
        addCharEntity(123, result);
        }
        else if (character == '|') {
        addCharEntity(124, result);
        }
        else if (character == '}') {
        addCharEntity(125, result);
        }
        else if (character == '~') {
        addCharEntity(126, result);
        }
        else {
        //the char is not a special one
        //add it to the result as is
        result.append(character);
        }
        character = iterator.next();
        }
        return result.toString();
        }

    private static void addCharEntity(Integer aIdx, StringBuilder aBuilder){
        String padding = "";
        if( aIdx <= 9 ){
        padding = "00";
        }
        else if( aIdx <= 99 ){
        padding = "0";
        }
        else {
        //no prefix
        }
        String number = padding + aIdx.toString();
        aBuilder.append("&#" + number + ";");
    }
}
