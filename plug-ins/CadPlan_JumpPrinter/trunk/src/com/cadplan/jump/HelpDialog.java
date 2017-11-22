package com.cadplan.jump;

import com.cadplan.designer.GridBagDesigner;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;

/**
 * User: geoff
 * Date: 20/02/2007
 * Time: 12:35:08
 * Copyright 2005 Geoffrey G Roy.
 */
public class HelpDialog extends JDialog implements ActionListener
{
     JButton cancelButton;
     JLabel label;
     String indent = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";


     String text;
     I18NPlug iPlug;

    public HelpDialog(JDialog parent, I18NPlug iPlug)
    {
        super(parent, iPlug.get("JumpPrinter.HelpDialog"), false);
        this.iPlug = iPlug;
        text = "<html>"+
                   "<b><h3>"+iPlug.get("JumpPrinter.HelpDialog.MouseOptions")+":</h3></b>" +
                   "<b>&lt;"+iPlug.get("JumpPrinter.HelpDialog.MoveOption")+"&gt;</b> - "+iPlug.get("JumpPrinter.HelpDialog.Message1")+"<br>"+
                   "<b>&lt;"+iPlug.get("JumpPrinter.HelpDialog.PanOption")+"&gt;</b> - "+iPlug.get("JumpPrinter.HelpDialog.Message2")+"<br>"+
                   "<b><h3>"+iPlug.get("JumpPrinter.HelpDialog.ButtonOptions")+":</h3></b>" +
                   "<b>[+]</b> - "+iPlug.get("JumpPrinter.HelpDialog.ZoomIn")+"<br>"+
                   "<b>[O]</b> - "+iPlug.get("JumpPrinter.HelpDialog.Zoom100")+"<br>"+
                   "<b>[-]</b> - "+iPlug.get("JumpPrinter.HelpDialog.ZoomOut")+"<br>"+
                   "<b>["+iPlug.get("JumpPrinter.Setup.LoadCfg")+"]</b> - "+iPlug.get("JumpPrinter.HelpDialog.Message3")+"<br>"+
                   "<b>["+iPlug.get("JumpPrinter.Setup.SaveCfg")+"]</b> - "+iPlug.get("JumpPrinter.HelpDialog.Message4")+"<br>"+
                   "<b>["+iPlug.get("JumpPrinter.Furniture")+"]</b> - "+iPlug.get("JumpPrinter.HelpDialog.Message5")+"<br>"+
                   "<b><h3>"+iPlug.get("JumpPrinter.HelpDialog.Options")+":</h3></b>" +
                   "<b>"+iPlug.get("JumpPrinter.Setup.SinglePage")+"</b> - "+iPlug.get("JumpPrinter.HelpDialog.Message6")+"<br>"+
                   "<b>"+iPlug.get("JumpPrinter.Setup.Quality")+"</b> - "+iPlug.get("JumpPrinter.HelpDialog.Message7")+"<br>"+
                   "<b><h3>"+iPlug.get("JumpPrinter.HelpDialog.InputFields")+":</h3></b>" +
                   "<b>"+iPlug.get("JumpPrinter.Setup.Scale")+"</b> - "+iPlug.get("JumpPrinter.HelpDialog.Message8")+"<br>"+
                   "</html>";
        init();
    }

    public void init()
    {
         GridBagDesigner gb = new GridBagDesigner(this);
         label = new JLabel(text);
         gb.setPosition(0,0);
         gb.setInsets(5,20,20,5);
         gb.addComponent(label);

         cancelButton = new JButton(iPlug.get("JumpPrinter.HelpDialog.Close"));
         gb.setPosition(0,1);
         gb.setInsets(0,0,10,0);
         gb.addComponent(cancelButton);
         cancelButton.addActionListener(this);
         pack();
         setVisible(true);
    }
    public void actionPerformed(ActionEvent ev)
    {
        if(ev.getSource() == cancelButton)
        {
            dispose();
        }
    }
}
