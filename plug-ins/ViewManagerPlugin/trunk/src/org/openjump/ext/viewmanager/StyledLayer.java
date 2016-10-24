package org.openjump.ext.viewmanager;

import com.vividsolutions.jump.workbench.model.*;
import com.vividsolutions.jump.workbench.ui.renderer.style.*;
import org.apache.log4j.Logger;
import org.openjump.ext.viewmanager.style.*;

import javax.xml.bind.annotation.*;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * A set of styles we want to apply to a Layer.
 */
@XmlRootElement
public class StyledLayer {

    final Logger LOG = Logger.getLogger(StyledLayer.class);

    @XmlAttribute
    String category;

    @XmlAttribute
    String layer;

    @XmlAttribute
    boolean visible = true;

    @XmlAttribute
    boolean editable = false;

    @XmlAttribute
    boolean selectable = true;

    @XmlElement
    PScale scale;

    @XmlElementWrapper
    @XmlAnyElement (lax=true)
    List<PStyle> styles;

    public StyledLayer() {
        addStyle(new PBasicStyle(new BasicStyle()));
        addStyle(new PLabelStyle(new LabelStyle()));
        //addStyle(new PColorThemingStyle(new ColorThemingStyle()));
    }

    public void addStyle(PStyle style) {
        if(styles == null) styles = new ArrayList<PStyle>();
        styles.add(style);
    }

    public void setBasicStyle(BasicStyle style) {
        if (styles != null) {
            List<PStyle> stylesToBeRemoved = new ArrayList<PStyle>();
            for (PStyle pstyle : this.styles) {
                if (pstyle instanceof PBasicStyle) {
                    stylesToBeRemoved.add(pstyle);
                }
            }
            styles.removeAll(stylesToBeRemoved);
        }
        addStyle(new PBasicStyle(style));
    }

    public void setVertexStyle(BasicStyle bStyle, VertexStyle vStyle) {
        if (styles != null) {
            List<PStyle> stylesToBeRemoved = new ArrayList<PStyle>();
            for (PStyle pstyle : this.styles) {
                if (pstyle instanceof PVertexStyle) {
                    stylesToBeRemoved.add(pstyle);
                }
            }
            styles.removeAll(stylesToBeRemoved);
        }
        addStyle(new PVertexStyle(bStyle, vStyle));
    }

    public void setLabelStyle(LabelStyle style) {
        if (styles != null) {
            List<PStyle> stylesToBeRemoved = new ArrayList<PStyle>();
            for (PStyle pstyle : this.styles) {
                if (pstyle instanceof PLabelStyle) {
                    stylesToBeRemoved.add(pstyle);
                }
            }
            styles.removeAll(stylesToBeRemoved);
        }
        addStyle(new PLabelStyle(style));
    }

    public void setColorThemingStyle(ColorThemingStyle style) {
        if (styles != null) {
            List<PStyle> stylesToBeRemoved = new ArrayList<PStyle>();
            for (PStyle pstyle : this.styles) {
                if (pstyle instanceof PColorThemingStyle) {
                    stylesToBeRemoved.add(pstyle);
                }
            }
            styles.removeAll(stylesToBeRemoved);
        }
        addStyle(new PColorThemingStyle(style));
        if (style.isEnabled()) {
            PStyle basicStyle = getStyle(PBasicStyle.class);
            if (basicStyle != null) {
                basicStyle.setEnabled(false);
            }
        }
    }

    public PStyle getStyle(Class clazz) {
        for (PStyle style : styles) {
            if (clazz.isInstance(style)) return style;
        }
        return null;
    }

    /**
     * Apply styles on layers matching the category/layer attributes
     * @param layerManager
     */
    public void applyStyles(LayerManager layerManager) {
        List<Category> categories = layerManager.getCategories();
        for (Category cat : categories) {
            List<Layerable> layerables = cat.getLayerables();
            for (Layerable lyr : layerables) {
                LOG.info(cat + "/" + lyr);
                if (accept(category, layer, cat, lyr)) {
                    LOG.info("accepted : apply styles");
                    applyStyles(lyr);
                }
            }
        }
    }

    /**
     * Remove all old styles of lyr and apply new styles.
     * @param lyr
     */
    public void applyStyles(Layerable lyr) {
        LOG.info("Apply styles to " + lyr);
        lyr.setVisible(visible);
        if (scale != null) {
            lyr.setScaleDependentRenderingEnabled(scale.scaleDependent);
            lyr.setMaxScale(scale.maxScale);
            lyr.setMinScale(scale.minScale);
        }
        if (lyr instanceof Layer) {
            Layer layer = (Layer) lyr;
            layer.setSelectable(selectable);
            layer.setEditable(editable);
            if (styles != null) {
                LOG.info("Nombre de styles enregistrés : " + styles.size());
                for (PStyle pstyle : styles) {
                    try {
                        Style newStyle = pstyle.getStyle(lyr);
                        Style rm = layer.getStyle(newStyle.getClass());
                        if (rm != null) {
                            layer.removeStyle(rm);
                        } else if (BasicStyle.class.isInstance(newStyle)) {
                            layer.removeStyle(layer.getBasicStyle());
                        }
                        LOG.info("  - replace " + newStyle.getClass().getSimpleName());
                        layer.addStyle(newStyle);
                    } catch (Exception e) {
                        LOG.warn(pstyle, e);
                    }
                }
            }
            layer.getLayerManager().fireLayerChanged(layer, LayerEventType.APPEARANCE_CHANGED);
        }
    }

    private boolean accept(String categoryFilter, String layerFilter, Category category, Layerable layer) {
        Pattern categoryPattern = Pattern.compile(".*");
        Pattern layerPattern = Pattern.compile(".*");
        try {
            categoryPattern = getPattern(categoryFilter);
            layerPattern = getPattern(layerFilter);
        } catch (PatternSyntaxException pse) {
                pse.printStackTrace();
            LOG.warn("", pse);
        }
        return categoryPattern.matcher(category.getName()).matches() &&
                layerPattern.matcher(layer.getName()).matches();
    }

    /**
     * Transform the category or layer attribute into a Pattern.
     * If the attribute starts and ends with a /, it IS already a regex
     * Else if the attribute contains a *, the * is interpreted as a glob
     * Else, the category or layer name must match exactly the attribute
     */
    private Pattern getPattern(String filter) throws PatternSyntaxException {
        Pattern pattern;
        if(filter == null) {
            pattern = Pattern.compile(".*");
        } else {
            // create a pattern to read this specific name
            pattern = Pattern.compile(Pattern.quote(filter));
            // then try to interpret * as glob
            if (filter.contains("*")) {
                pattern = Pattern.compile(filter.replaceAll("\\*", ".*"));
            }
            // and finally, try to read layer name as a regex if it starts and ends with a /
            if (filter.startsWith("/") && filter.endsWith("/")) {
                pattern = Pattern.compile(filter.substring(1, filter.length() - 1));
            }
        }
        return pattern;
    }

    public String toString() {
        return category + " / " + layer + " (" + visible + "/" + editable + "/" + selectable + ") : " + styles;
    }

}
