package com.cadplan.jump;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.border.BevelBorder;
import javax.swing.border.TitledBorder;



import com.cadplan.designer.GridBagDesigner;
import com.cadplan.fileio.*;
import com.cadplan.fileioA.FileChooser;

public class FurnitureImagePanel extends JPanel implements ActionListener, AdjustmentListener
{
	
    Vector<FurnitureImage> imageItems;
    JPanel imagesPanel;
    
    JCheckBox [] showImagesCB;
    JCheckBox [] ratioLockedCB;
    JTextField [] fileNamesField;
    
    JButton [] deleteButton;
    JButton [] browseButton;
    JButton addButton;
    JLabel showLabel, fileNameLabel,  lockedLabel;
    
    I18NPlug iPlug;
    GridBagDesigner gbp, gb;
    
    FurnitureDialog parent;
    JScrollPane scrollPane;
    JScrollBar vsb;
    JLabel layerLabel;
    JTextField layerField;
    JTextField [] layerFields;
    
    int defaultImageMax = 300;

    public FurnitureImagePanel(FurnitureDialog parent, Vector<FurnitureImage> imageItems, I18NPlug iPlug)
    {
        this.imageItems = imageItems;
        this.parent = parent;
        this.iPlug = iPlug;
        init();
        setImages();
        
    }


    private void setImages()
    {
         
         
         for(int i=0; i < imageItems.size(); i++)
         {
        	 FurnitureImage imageItem = imageItems.elementAt(i);
        	 showImagesCB[i].setSelected(imageItem.show);
        	 fileNamesField[i].setText(imageItem.fileName);
        	 ratioLockedCB[i].setSelected(imageItem.ratioLocked);
        	 
        	 layerFields[i].setText(String.valueOf(imageItem.layerNumber));
        	 //System.out.println("Setting border:"+aborder.show+","+aborder.fixed+","+aborder.thickness);
         }

    }

   
    public Vector<FurnitureImage> getImageItems()
    {
    	
    	for(int i=0; i < imageItems.size(); i++)
    	{
    		FurnitureImage imageItem = imageItems.elementAt(i);
    		imageItem.show = showImagesCB[i].isSelected();
    		imageItem.fileName = fileNamesField[i].getText();
    		imageItem.ratioLocked = ratioLockedCB[i].isSelected();
            
    		try
    		{
               imageItem.layerNumber = Integer.parseInt(layerFields[i].getText());
    		}
    		catch(NumberFormatException ex)
    		{
    			JOptionPane.showMessageDialog(this,iPlug.get("JumpPrinter.Furniture.Message2")+" "+i+": "+layerFields[i].getText(),
     	    			iPlug.get("JumpPrinter.Error"), JOptionPane.ERROR_MESSAGE);
    		}
            //System.out.println("Getting border:"+aborder.show+","+aborder.fixed+","+aborder.thickness);
    	}
    	return imageItems;
    }
    public void init()
    {
        gb = new GridBagDesigner(this);

        showLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Show"));
        gb.setPosition(0,0);
        gb.setInsets(10,20,0,0);
        //gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setWeight(0.0,0.0);
        gb.addComponent(showLabel);

       
        
        fileNameLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Image.ImageFile"));
        gb.setPosition(1,0);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setWeight(1.0,0.0);
        gb.addComponent(fileNameLabel);
        
        layerLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Layer"));
        gb.setPosition(3,0);
        gb.setInsets(10,10,0,0);
        gb.setAnchor(GridBagConstraints.EAST);
        gb.setWeight(1.0,0.0);
        gb.addComponent(layerLabel);
        
        lockedLabel = new JLabel(iPlug.get("JumpPrinter.Furniture.Image.Locked"));
        gb.setPosition(4,0);
        gb.setInsets(10,10,0,80);
        gb.setSpan(2,1);
        gb.setAnchor(GridBagConstraints.WEST);
        //gb.setWeight(1.0,0.0);
        gb.addComponent(lockedLabel);
        
        
        
        imagesPanel = new JPanel();
        //bordersPanel.setBorder(titledBorder);
        //gbp = new GridBagDesigner(bordersPanel);
        setupImagesPanel();
        
        createScrollPane();
        
        
        
        addButton = new JButton(iPlug.get("JumpPrinter.Furniture.Add"));
        gb.setPosition(5,4);
        gb.setInsets(10,0,10,10);
        gb.setAnchor(GridBagConstraints.SOUTHEAST);
        gb.addComponent(addButton);
        addButton.addActionListener(this);
       
        
    }
    
    private void createScrollPane()
    {
    	scrollPane = new JScrollPane(imagesPanel); 
    	
    	
    	//scrollPane.add(bordersPanel);
        scrollPane.setPreferredSize(new Dimension(400,200));
        gb.setPosition(0,3);
        gb.setInsets(5,10,10,10);
        gb.setAnchor(GridBagConstraints.NORTHWEST);
        gb.setFill(GridBagConstraints.BOTH);
        gb.setWeight(1.0,1.0);
        gb.setSpan(6,1);
        gb.addComponent(scrollPane);
        
        vsb = scrollPane.getVerticalScrollBar();
        vsb.addAdjustmentListener(this);
    }
    
    private void setupImagesPanel()
    {
    	imagesPanel = new JPanel();
    	gbp = new GridBagDesigner(imagesPanel);
    	int n = imageItems.size();
        showImagesCB = new JCheckBox[n];
        fileNamesField = new JTextField[n];
        deleteButton = new JButton[n];
        browseButton = new JButton[n];
        layerFields = new JTextField[n];
        ratioLockedCB = new JCheckBox[n];
        //System.out.println("Number of borders="+n);
        for(int i=0; i < imageItems.size(); i++)
        {
        	
        	showImagesCB[i] = new JCheckBox("");
        	gbp.setPosition(0,i);
        	gbp.setInsets(5,10,0,0);
        	gbp.setAnchor(GridBagConstraints.NORTHWEST);
        	gbp.addComponent(showImagesCB[i]);
        	
        	fileNamesField[i] = new JTextField(30);
            gbp.setPosition(1,i);
            gbp.setInsets(10,5,0,0);
            gbp.setAnchor(GridBagConstraints.WEST);
            gbp.setWeight(1.0,0.0);
            gbp.setFill(GridBagConstraints.HORIZONTAL);
            gbp.addComponent(fileNamesField[i]);
            fileNamesField[i].setEditable(false);
        	
            browseButton[i] = new JButton("...");
        	gbp.setPosition(2,i);
        	gbp.setInsets(10,0,0,0);
        	gbp.setAnchor(GridBagConstraints.NORTHWEST);
        	//gbp.setWeight(1.0,0.0);
        	gbp.addComponent(browseButton[i]);
        	browseButton[i].addActionListener(this);
        	
        	layerFields[i] = new JTextField(3);
            gbp.setPosition(3,i);
            gbp.setInsets(10,5,0,0);
            gbp.setAnchor(GridBagConstraints.WEST);
            //gb.setWeight(1.0,0.0);
            gbp.addComponent(layerFields[i]);
            
            ratioLockedCB[i] = new JCheckBox("");
            gbp.setPosition(4,i);
            gbp.setInsets(10,5,0,0);
            //gbp.setAnchor(GridBagConstraints.WEST);
            //gb.setWeight(1.0,0.0);
            gbp.addComponent(ratioLockedCB[i]);
        	
        	deleteButton[i] = new JButton(iPlug.get("JumpPrinter.Furniture.Delete"));
        	gbp.setPosition(5,i);
        	gbp.setInsets(10,5,0,10);
        	gbp.setAnchor(GridBagConstraints.NORTHWEST);
        	//gbp.setWeight(1.0,0.0);
        	gbp.addComponent(deleteButton[i]);
        	deleteButton[i].addActionListener(this);
        	
        }
        //gb.setPosition(0,3);
        //gb.setInsets(10,10,10,10);
        //gb.setAnchor(GridBagConstraints.NORTHWEST);
        //gb.setFill(GridBagConstraints.BOTH);
        //gb.setWeight(1.0,1.0);
        //gb.setSpan(4,1);
        //gb.addComponent(bordersPanel);
    }
    
    public void actionPerformed(ActionEvent ev)
    {
    	
    	if(ev.getSource() == addButton)
    	{
    		
    		imageItems = getImageItems();
    		FurnitureImage imageItem = new FurnitureImage("",true);
    		//imageItem.setImage(0, 200, 100, 50, true);
    		imageItems.addElement(imageItem);
    		
    		remove(scrollPane);
            setupImagesPanel();
            createScrollPane();
            parent.pack();
            
            setImages();
            scrollPane.repaint();
            
            
    		
    	}
    	
    	for(int i=0; i < imageItems.size(); i++)
    	{
    		if(ev.getSource() == deleteButton[i])
    		{
    			//System.out.println("Removing border: "+i);
    			imageItems.removeElementAt(i);
    			//borders = getBorders();
    			remove(scrollPane);
    			imagesPanel = new JPanel();
        		//bordersPanel.setBorder(titledBorder);
        		gbp = new GridBagDesigner(imagesPanel);
                setupImagesPanel();
                createScrollPane();
                //scrollPane.add(bordersPanel);
                parent.pack();
                
                setImages();
                return;
    			
    		}
    		
    		if(ev.getSource() == browseButton[i])
    		{
    			String [] types = new String[] {"jpg","jpeg","gif","png","svg"};
    			FileChooser fc = new FileChooser(this,null,null, types,
    					       "Select image file",JFileChooser.OPEN_DIALOG);
    			String dirName = fc.getDir();
    			String fileName = fc.getFile();
    			fileNamesField[i].setText(dirName+File.separator+fileName);
    			imageItems.elementAt(i).fileName = dirName+File.separator+fileName;
    			ImageLoader loader = new ImageLoader();
    			imageItems.elementAt(i).image = loader.loadImage(dirName+File.separator+fileName);
    			int w = imageItems.elementAt(i).image.getWidth(null);
    			int h = imageItems.elementAt(i).image.getHeight(null);
    			double ratio = (double)w/(double)h;
    			//System.out.println("Image0: w="+w+"  h="+h+"  ratio="+ratio);
    			if(w >= h)
    			{
    				if(w > defaultImageMax) 
    				{
    					w = defaultImageMax;
    					h = (int) ((double) w/ratio);
    				}
    				
    			}
    			else
    			{
    				if(h > defaultImageMax)
    				{
    					h = defaultImageMax;
    					w = (int) ((double) h*ratio);
    				}
    				
    			}
    			//System.out.println("Image2: w="+w+"  h="+h+"  ratio="+ratio);
    			imageItems.elementAt(i).location.x = 0;
    			imageItems.elementAt(i).location.y = 0;
    			imageItems.elementAt(i).location.width = w;
    			imageItems.elementAt(i).location.height =h;
    			
    			return;
    		}
    	}
    }
    public  void adjustmentValueChanged(AdjustmentEvent ev)
    {
    	
    	if(ev.getSource() == vsb)
    	{
    		
    		scrollPane.repaint();
    		parent.repaint();
    	}
    }
    
   
   
}
