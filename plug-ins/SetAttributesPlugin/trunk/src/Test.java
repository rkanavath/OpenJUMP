import com.vividsolutions.jump.task.DummyTaskMonitor;
import com.vividsolutions.jump.util.Blackboard;
import com.vividsolutions.jump.workbench.JUMPWorkbench;
import com.vividsolutions.jump.workbench.JUMPWorkbenchContext;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.driver.DriverManager;
import com.vividsolutions.jump.workbench.model.LayerManager;
import com.vividsolutions.jump.workbench.model.Task;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.registry.Registry;
import com.vividsolutions.jump.workbench.ui.ErrorHandler;
import com.vividsolutions.jump.workbench.ui.FeatureTextWriterRegistry;
import com.vividsolutions.jump.workbench.ui.LayerNamePanel;
import com.vividsolutions.jump.workbench.ui.LayerViewPanel;
import org.openjump.ext.setattributes.SetAttribute;
import org.openjump.ext.setattributes.SetAttributesToolbox;
import org.openjump.ext.setattributes.SetOfAttributes;

import javax.swing.*;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import java.io.File;

/**
 * Created by UMichael on 18/04/2015.
 */
public class Test {

    public static void main(String[] args) {

        File xml = new File("test/test.xml");
        try {
            JAXBContext jc = JAXBContext.newInstance(SetAttributesToolbox.class);
            Unmarshaller unmarshaller = jc.createUnmarshaller();
            SetAttributesToolbox toolbox = (SetAttributesToolbox) unmarshaller.unmarshal(new File("test/test.xml"));
            System.out.println(toolbox);
            System.out.println(toolbox.getTitle());
            System.out.println(toolbox.getButtons());
            for (SetOfAttributes button : toolbox.getButtons()) {
                System.out.println("  " + button);
                System.out.println("  " + button.getIcon());
                System.out.println("  " + button.getLayer());
                System.out.println("  " + button.isAtomic());
                System.out.println("  " + button.getAttributes());
                for (SetAttribute attribute : button.getAttributes()) {
                    System.out.println("    " + attribute.getName());
                    System.out.println("    " + attribute.getValue());
                }
            }
            //JDialog dialog = toolbox.createDialog(new JUMPWorkbenchContext(new JUMPWorkbench("", new String[0], new JFrame(), new DummyTaskMonitor())));
            JDialog dialog = toolbox.createDialog(new WorkbenchContext() {}, new File("."));
            dialog.pack();
            dialog.setVisible(true);


        } catch(Exception e) {
            e.printStackTrace();
        }

    }

}
