package com.cadplan.jump;

import javax.swing.*;
import javax.swing.event.HyperlinkListener;
import javax.swing.event.HyperlinkEvent;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.ImageFilter;
import java.awt.image.ImageProducer;
import java.awt.image.FilteredImageSource;

/**
 * User: geoff
 * Date: 2/08/2007
 * Time: 17:42:07
 * Copyright 2005 Geoffrey G Roy.
 */
public class HTMLTextComponent extends Component
{
    boolean debug = false;
    String text;
    double offset;
    float x0;
    float y0;
    Color backgroundColor;
    Color foregroundColor;
    double alpha;
    Image timage;
    public int imageWidth;
    public int imageHeight;
    int border;
    int position;
    boolean fillText;
    String fontName;
    int fontSize;
    double scaleFactor = 4.0;
    MediaTracker mediaTracker = new MediaTracker(this);;


    public HTMLTextComponent(String text, double offset, float x0, float y0, Color backgroundColor,
                             Color foregroundColor, double alpha, int border, int position, boolean fillText,
                             String fontName, int fontSize)
    {
        this.text = text;
        this.offset = offset;
        this.x0 = x0;
        this.y0 = y0;
        this.backgroundColor = backgroundColor;
        this.foregroundColor = foregroundColor;
        this.alpha = alpha;
        this.border = border;
        this.position = position;
        this.fillText = fillText;
        this.fontName = fontName;
        this.fontSize = fontSize;
        
        timage = createTextImage();
    }

    private Image createTextImage()
    {
        JWindow frame = new JWindow();

        final JEditorPane editorPane = new JEditorPane();

        editorPane.setForeground(foregroundColor);
        Font font = new Font(fontName,Font.PLAIN, (int) (fontSize*scaleFactor));
        editorPane.setFont(font);
        editorPane.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES,true);

        editorPane.setBackground(Color.WHITE);

        editorPane.setDoubleBuffered(false);
        editorPane.setEditable(false);
        editorPane.setContentType("text/html");
        editorPane.setText(text);
        Rectangle bounds = editorPane.getBounds();
        if(debug) System.out.println("Painting text: "+text);
        frame.setContentPane(editorPane);
        frame.pack();
        Dimension size = editorPane.getSize();
        BufferedImage image = new BufferedImage(size.width,size.height, BufferedImage.TYPE_INT_RGB);

        final Graphics2D graphics = image.createGraphics();
//        graphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
//        graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
//        graphics.setRenderingHint(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_DISABLE);
//        graphics.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);
//        graphics.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY);


           editorPane.paint( graphics );
          mediaTracker.addImage(image,0);
          try
          {
                mediaTracker.waitForID(0);
          }
          catch (InterruptedException e)
          {
                return null;
          }

//          JLabel label = new JLabel(text);
//          Font font = new Font(fontName,Font.PLAIN, fontSize);
//          label.setFont(font);
//          label.setForeground(foregroundColor);
//          label.setBackground(Color.WHITE);
//          label.setOpaque(false);
//          frame.setContentPane(label);
//          frame.pack();
//          Dimension size = label.getSize();
//          BufferedImage image = new BufferedImage(size.width,size.height, BufferedImage.TYPE_INT_RGB);
//          Graphics2D graphics = image.createGraphics();
//          label.paint(graphics);

        //g.translate(x0,y0);
//        JPanel panel = new JPanel();
//        panel.add(editorPane);
//        panel.setVisible(true);
//        //Image image = panel.createImage(100,100);
//        final JButton button = new JButton("Hello");
//        button.addNotify();
//        Image image = button.createImage(100,100);
//        final Graphics graphics = image.getGraphics();
//        try
//        {
//            SwingUtilities.invokeAndWait(new Runnable()
//            {
//                public void run()
//                {
//                    editorPane.paint(graphics);
//                }
//            });
//
//        }
//        catch (Exception ex)
//        {
//            ex.printStackTrace();
//        }
        if(image == null)
        {
            if(debug) System.out.println("Image is null");
            return null;
        }
        //if(!fillText) alpha = 0.0;
        alpha = 0.0;  // make fully transparent now that we fill the backgrousn first.
        Image timage = makeTransparent(image,Color.WHITE, alpha);
        //Image timage = image;
        //Graphics tgraphics = timage.getGraphics();
         
        if(debug) System.out.println("TImage size: "+timage.getWidth(null)+","+timage.getHeight(null));
        imageWidth = (int) (timage.getWidth(null)/scaleFactor);
        imageHeight = (int) (timage.getHeight(null)/scaleFactor);
        return timage;
    }

    public void paint(Graphics g, float x, float y, double drawFactor)
    {
        int width = imageWidth; //(int) (imageWidth*drawFactor);
        int height =  imageHeight; //(int) (imageHeight*drawFactor);

         LabelCallout callout = new LabelCallout(g, border, position, (int)x0, (int) y0,
                    (int) x, (int) y , width-1, height-1,foregroundColor, backgroundColor,fillText );
        Graphics2D g2 = (Graphics2D) g;
//        g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
//        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
//        g2.setRenderingHint(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_DISABLE);
//        g2.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);

        g2.drawImage(timage,(int)x,(int)y, width, height, null);
        //System.out.println("Painting image:");
    }

    private Image makeTransparent(Image image,  Color color, double alpha)
    {
          TransparencyFilter filter = new TransparencyFilter(color, alpha);
          ImageProducer ip = new FilteredImageSource(image.getSource(), filter);

          Image timage = Toolkit.getDefaultToolkit().createImage(ip);
          mediaTracker.addImage(timage,2);
          try
          {
                mediaTracker.waitForID(2);
          }
          catch (InterruptedException e)
          {
                return null;
          }
          
          return timage;
    }

   
}
