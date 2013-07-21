package org.openjump.core.ui.plugin.layer.raster;



import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.color.ColorSpace;
import java.awt.color.ICC_ColorSpace;
import java.awt.color.ICC_Profile;
import java.awt.image.BufferedImage;
import java.awt.image.ColorConvertOp;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageFilter;
import java.awt.image.ImageProducer;
import java.awt.image.RGBImageFilter;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;
import javax.imageio.ImageIO;
import org.apache.commons.imaging.ImageFormat;
import org.apache.commons.imaging.ImageWriteException;
import org.apache.commons.imaging.Sanselan;
import org.apache.commons.imaging.SanselanConstants;
import org.apache.commons.imaging.formats.png.PngConstants;
import org.apache.commons.imaging.formats.tiff.constants.TiffConstants;
import org.apache.log4j.Logger;
import org.openjump.core.rasterimage.RasterImageLayer;


public class ImageUtils {

    /** Log */
    private final static Logger LOGGER = Logger.getLogger(ImageUtils.class);

    
   
    
    /**
     * Writes a buffered image to a output stream, as format JPG2000
     * Currently deactivated
     * an attempt to use jai-imageio-core that allows to write to JP2
     * https://github.com/stain/jai-imageio-core 
     * @param out
     * @param quality
     * @param image
     * @throws IOException
     */
    public static void writeBufferedImageAsJPEG2000( OutputStream out, 
            BufferedImage image ) throws IOException {
        if (out == null) {
            throw new IllegalArgumentException("Null 'out' argument."); //$NON-NLS-1$
        }
        if (image == null) {
            throw new IllegalArgumentException("Null 'image' argument."); //$NON-NLS-1$
        }
        ImageIO.write(image, "jpeg 2000", out); //$NON-NLS-1$
    }
    
    /**
     * Writes a buffered image to a output stream, as format JPG 
     * and with the indicated quality
     * Converting buffered image to RGB
     * @param out
     * @param quality
     * @param image
     * @throws IOException
     * @throws ImageWriteException 
     */
    public static void writeBufferedImageAsJPEG( OutputStream out, float quality,
            BufferedImage image ) throws IOException, ImageWriteException {
        if (out == null) {
            throw new IllegalArgumentException("Null 'out' argument."); //$NON-NLS-1$
        }
        if (image == null) {
            throw new IllegalArgumentException("Null 'image' argument."); //$NON-NLS-1$
        }
        ICC_Profile iccProfile = ICC_Profile.getInstance(ICC_ColorSpace.CS_sRGB);
        ColorSpace cSpace = new ICC_ColorSpace(iccProfile);
        ColorConvertOp op = new ColorConvertOp(image.getColorModel().getColorSpace(), cSpace, null);
        BufferedImage newImage = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.OPAQUE);
        op.filter(image, newImage);
       ImageIO.write(newImage, "jpg", out); //$NON-NLS-1$
    }

    
    /**
     * Writes a buffered image to a output stream, as format PNG 
     * TODO: Add compression level and color transparency
     * 
     * @param out
     * @param image
     * @param transparent
     * @throws IOException
     * @throws ImageWriteException 
     */
    public static void writeBufferedImageAsPNG( OutputStream out, BufferedImage image ) throws IOException, ImageWriteException {
        if (out == null) {
            throw new IllegalArgumentException("Null 'out' argument."); //$NON-NLS-1$
        }
        if (image == null) {
            throw new IllegalArgumentException("Null 'image' argument."); //$NON-NLS-1$
        }
      ImageIO.write(image, "png", out); //$NON-NLS-1$
    }

    
   
    
    /**
     * Writes a buffered image to a output stream, as format BMP 
     * Converting buffered image to RGB
     * @param out
     * @param image
     * @throws IOException
     * @throws ImageWriteException 
     */
    public static void writeBufferedImageAsBMP( OutputStream out, BufferedImage image )
            throws IOException, ImageWriteException {
        if (out == null) {
            throw new IllegalArgumentException("Null 'out' argument."); //$NON-NLS-1$
        }
        if (image == null) {
            throw new IllegalArgumentException("Null 'image' argument."); //$NON-NLS-1$
        }
        ICC_Profile iccProfile = ICC_Profile.getInstance(ICC_ColorSpace.CS_sRGB);
        ColorSpace cSpace = new ICC_ColorSpace(iccProfile);
        ColorConvertOp op = new ColorConvertOp(image.getColorModel().getColorSpace(), cSpace, null);
        BufferedImage newImage = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.OPAQUE);
        op.filter(image, newImage);
        ImageIO.write(newImage, "bmp", out); //$NON-NLS-1$
    }
   
    

    /**
     * Writes a buffered image to a output stream, as format TIF
     * Using common-imaging jar library from Apache (this library works with OpenJDK) 
     * @param out
     * @param image
     * @throws IOException
     * @throws ImageWriteException 
     */
    public static void writeBufferedImageAsTIF( OutputStream out, BufferedImage image )
            throws IOException, ImageWriteException {
        if (out == null) {
            throw new IllegalArgumentException("Null 'out' argument."); //$NON-NLS-1$
        }
        if (image == null) {
            throw new IllegalArgumentException("Null 'image' argument."); //$NON-NLS-1$
        }

        ImageFormat format = ImageFormat.IMAGE_FORMAT_TIFF;
        Map<String,Object> params = new HashMap<String,Object>();
        params.put(SanselanConstants.PARAM_KEY_COMPRESSION, new Integer(
           TiffConstants.TIFF_COMPRESSION_PACKBITS));
 
         Sanselan.writeImage(image, out, format, params);
    
            }
    
    

    /**
     * Writes a buffered image to a output stream, as format PNG and with 
     * the indicated transparency for color white
     * Not used
     * 
     * @param out
     * @param image
     * @param transparent
     * @throws IOException
     */
    
    
    public static void writeBufferedImageAsPNG2( OutputStream out, BufferedImage image ) throws IOException {
        if (out == null) {
            throw new IllegalArgumentException("Null 'out' argument."); //$NON-NLS-1$
        }
        if (image == null) {
            throw new IllegalArgumentException("Null 'image' argument."); //$NON-NLS-1$
        }
        
        Image transpImg2 = TransformColorToTransparency(image, Color.WHITE);
        BufferedImage resultImage2 = ImageToBufferedImage(transpImg2, image.getWidth(), image.getHeight());

        ImageIO.write(resultImage2, "png", out); //$NON-NLS-1$
    }
    
    
    private static BufferedImage ImageToBufferedImage(Image image, int width, int height)
    {
      BufferedImage dest = new BufferedImage(
          width, height, BufferedImage.TYPE_INT_ARGB);
      Graphics2D g2 = dest.createGraphics();
      g2.drawImage(image, 0, 0, null);
      g2.dispose();
      return dest;
    }
    
    private static Image TransformColorToTransparency(BufferedImage image, Color c1)
    {
      // Primitive test, just an example
      final int r1 = c1.getRed();
      final int g1 = c1.getGreen();
      final int b1 = c1.getBlue();
     
      ImageFilter filter = new RGBImageFilter()
      {
        public final int filterRGB(int x, int y, int rgb)
        {
          int r = (rgb & 0xFF0000) >> 16;
          int g = (rgb & 0xFF00) >> 8;
          int b = rgb & 0xFF;
          if (r >= r1  &&
              g >= g1   &&
              b >= b1  )
          {
              // Set fully transparent but keep color
              return rgb & 0xFFFFFF;
            }
            return rgb;
          }
        };

        ImageProducer ip = new FilteredImageSource(image.getSource(), filter);
          return Toolkit.getDefaultToolkit().createImage(ip);
      }
    
    /**
     * End of Code
     */
}