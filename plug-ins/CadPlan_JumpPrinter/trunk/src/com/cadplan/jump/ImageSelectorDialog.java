package com.cadplan.jump;
import java.awt.*;
import javax.swing.*;

import java.awt.event.*;
import com.cadplan.designer.GridBagDesigner;

public class ImageSelectorDialog extends JDialog implements ActionListener
{
	private JLabel typeLabel, sizeLabel, qualityLabel, byLabel, svgLabel;
	private JComboBox typeCombo;
	private JTextField xField, yField, svgField;
	private JCheckBox aspectCB;
	private JSlider qualitySlider;
	JButton cancelButton, okButton;
	private String [] typeOptions = {"JPG","PNG","SVG","PDF"};
	public int xSize, ySize;
	public int quality;
	public String type = "JPG";
	public boolean cancelled = false;
	private I18NPlug iPlug;
	public double svgFactor = 1.0;
	

	public ImageSelectorDialog(int xSize, int ySize, I18NPlug iPlug)
	{
		super(new JFrame(), iPlug.get("JumpPrinter.Image.Dialog"), true);
		this.xSize = xSize;
		this.ySize = ySize;
		this.iPlug = iPlug;
		
		init();
	}
	
	
	public void init()
	{
		GridBagDesigner gb = new GridBagDesigner(this);
		
		typeLabel = new JLabel(iPlug.get("JumpPrinter.Image.ImageType"));
		gb.setPosition(0,0);
		gb.setInsets(10, 10,0,0);
		gb.setSpan(3,1);
		gb.setAnchor(GridBagConstraints.WEST);
		gb.addComponent(typeLabel);
		
		typeCombo = new JComboBox(typeOptions);
		gb.setPosition(3,0);
		gb.setInsets(10, 10,0,10);
		gb.setSpan(1,1);
		gb.setFill(GridBagConstraints.HORIZONTAL);
		gb.addComponent(typeCombo);
		typeCombo.addActionListener(this);
		
		sizeLabel = new JLabel(iPlug.get("JumpPrinter.Image.Size"));
		gb.setPosition(0,1);
		gb.setInsets(10, 10,0,0);
		gb.setSpan(1,1);
		gb.setAnchor(GridBagConstraints.WEST);
		gb.addComponent(sizeLabel);
		
		xField = new JTextField(6);
		gb.setPosition(1,1);
		gb.setInsets(10, 10,0,0);
		gb.setSpan(1,1);
		gb.addComponent(xField);
		xField.addActionListener(this);
		
		byLabel = new JLabel("x");
		gb.setPosition(2,1);
		gb.setInsets(10, 10,0,0);
		gb.setSpan(1,1);
		gb.addComponent(byLabel);
		
		yField = new JTextField(6);
		gb.setPosition(3,1);
		gb.setInsets(10, 10,0,10);
		gb.setSpan(1,1);
		gb.addComponent(yField);
		yField.addActionListener(this);
		
		aspectCB = new JCheckBox(iPlug.get("JumpPrinter.Image.KeepAspectRatio"));
		gb.setPosition(1,2);
		gb.setInsets(10, 10,0,10);
		gb.setSpan(3,1);
		gb.setAnchor(GridBagConstraints.WEST);
		gb.addComponent(aspectCB);
		
		qualityLabel = new JLabel(iPlug.get("JumpPrinter.Image.JPEGQuality"));
		gb.setPosition(0,3);
		gb.setInsets(10, 10,0,0);
		gb.setSpan(3,1);
		gb.setAnchor(GridBagConstraints.WEST);
		gb.addComponent(qualityLabel);
		
		qualitySlider = new JSlider(0, 100, 90);
		qualitySlider.setPaintLabels(true);
		qualitySlider.setPaintTicks(true);
		qualitySlider.setMajorTickSpacing(10);
		qualitySlider.setSnapToTicks(true);
		
		gb.setPosition(0,4);
		gb.setInsets(2, 10,0,10);
		gb.setSpan(4,1);
		gb.setFill(GridBagConstraints.HORIZONTAL);
		gb.addComponent(qualitySlider);
		
		
		svgLabel = new JLabel("SVG scale factor");
		gb.setPosition(0,5);
		gb.setInsets(10, 10,0,0);
		gb.setSpan(2,1);
		gb.addComponent(svgLabel);
		
		svgField = new JTextField(8);
		gb.setPosition(2,5);
		gb.setInsets(10, 10,0,0);
		gb.setSpan(2,1);
		//gb.setFill(GridBagConstraints.HORIZONTAL);
		gb.addComponent(svgField);
		svgField.setText("1.0");
		svgField.setEnabled(false);
		
		
		cancelButton = new JButton(iPlug.get("JumpPrinter.Setup.Cancel"));
		gb.setPosition(0,6);
		gb.setInsets(10, 10,10,0);
		gb.setSpan(1,1);
		gb.addComponent(cancelButton);
		cancelButton.addActionListener(this);
		
		okButton = new JButton(iPlug.get("JumpPrinter.Setup.OK"));
		gb.setPosition(3,6);
		gb.setInsets(10, 10,10,10);
		gb.setSpan(1,1);
		gb.addComponent(okButton);
		okButton.addActionListener(this);
		
		addData();
		pack();
		setLocation(100,100);
		setVisible(true);
	}
	
	private void addData()
	{
		xField.setText(String.valueOf(xSize));
		yField.setText(String.valueOf(ySize));
		aspectCB.setSelected(true);
	}
	
	private void getData()
	{
		type = (String) typeCombo.getSelectedItem();
		quality = qualitySlider.getValue();
		try
		{
			svgFactor = Double.parseDouble(svgField.getText());
		}
		catch (NumberFormatException ex)
		{
			JOptionPane.showMessageDialog(this,"Error in SVG Scale Factor field: "+svgField.getText()+".  Taken as 1.0","Error...", 
					JOptionPane.ERROR_MESSAGE);
			svgFactor = 1.0;
		}
		cancelled = false;
	}
	private void enableFields(int type)
	{
		switch (type)
		{
		
		case 0:	xField.setEnabled(true);	//JPG
				yField.setEnabled(true);
				aspectCB.setEnabled(true);
				qualitySlider.setEnabled(true);
				svgField.setEnabled(false);
				break;
		case 1:	xField.setEnabled(true);	//PNG
				yField.setEnabled(true);
				aspectCB.setEnabled(true);
				qualitySlider.setEnabled(false);
				svgField.setEnabled(false);
				break;
		case 2:	xField.setEnabled(false);	//SVG
				yField.setEnabled(false);
				aspectCB.setEnabled(false);
				qualitySlider.setEnabled(false);
				svgField.setEnabled(true);
				break;
		case 3:	xField.setEnabled(false);	//PDF
				yField.setEnabled(false);
				aspectCB.setEnabled(false);
				qualitySlider.setEnabled(false);
				svgField.setEnabled(false);
				break;
		
		}
		
	}
	
	public void actionPerformed(ActionEvent ev)
	{
		
		
		if(ev.getSource() == cancelButton)
		{
			cancelled= true;
			dispose();
		}
		if(ev.getSource() == okButton)
		{
			getData();
			dispose();
		}
		
		if(ev.getSource() == typeCombo)
		{
			enableFields(typeCombo.getSelectedIndex());
			
			
		}
		
		if(ev.getSource() == xField)
		{
			String sval = xField.getText();
			int xval = xSize;
			int yval = ySize;
			try
			{
				xval = Integer.parseInt(sval);
			}
			catch (NumberFormatException ex)
			{
				JOptionPane.showMessageDialog(this,"Number format error in X size","Error...",JOptionPane.ERROR_MESSAGE);
				return;
			}
			if(xval <= 0)
			{
				JOptionPane.showMessageDialog(this,"Number format error in X size","Error...",JOptionPane.ERROR_MESSAGE);
				return;
			}
			if(aspectCB.isSelected())
			{
				yval = xval*ySize/xSize;
				yField.setText(String.valueOf(yval));
			}
			xSize = xval;
			ySize = yval;
		}
		
		if(ev.getSource() == yField)
		{
			String sval = yField.getText();
			int yval = ySize;
			int xval = xSize;
			try
			{
				yval = Integer.parseInt(sval);
			}
			catch (NumberFormatException ex)
			{
				JOptionPane.showMessageDialog(this,"Number format error in Y size","Error...",JOptionPane.ERROR_MESSAGE);
				return;
			}
			if(yval <= 0)
			{
				JOptionPane.showMessageDialog(this,"Number format error in Y size","Error...",JOptionPane.ERROR_MESSAGE);
				return;
			}
			if(aspectCB.isSelected())
			{
				xval = yval*xSize/ySize;
				xField.setText(String.valueOf(xval));
			}
			xSize = xval;
			ySize = yval;
		}
	}
}
