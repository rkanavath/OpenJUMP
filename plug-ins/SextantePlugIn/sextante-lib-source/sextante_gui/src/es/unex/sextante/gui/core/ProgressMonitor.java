package es.unex.sextante.gui.core;

import info.clearthought.layout.TableLayout;
import info.clearthought.layout.TableLayoutConstants;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.List;

import javax.swing.BoundedRangeModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.text.DefaultStyledDocument;

import es.unex.sextante.core.Sextante;
import es.unex.sextante.gui.core.SextanteGUI;
import es.unex.sextante.gui.settings.SextanteGeneralSettings;
import es.unex.sextante.gui.toolbox.TransparentScrollPane;


/**
 * An advanced progress monitor with progress bar and a message
 * text area. The message text area is added as required.
 * If the monitored algorithm does not output any additional
 * status text, then the monitor will behave as in previous
 * versions of SEXTANTE. 
 *
 * @author volaya
 * @author benducke
 *
 */
public class ProgressMonitor
extends
JDialog {

	private static final long serialVersionUID = 1L;

	private static final int DIM_WIDTH_DEFAULT = 500;
	private static final int DIM_HEIGHT_DEFAULT = 130;
	private static final int DIM_WIDTH_VERBOSE = 640;
	private static final int DIM_HEIGHT_VERBOSE = 480;
	private static final int DIM_HEIGHT_ROW_DEFAULT = 20;

	private static final int[] TEXT_COLOR_BACK = {229,228,226};

	private static final String TEXT_COLOR_INFO = "black";
	private static final String TEXT_COLOR_WARNING = "blue";
	private static final String TEXT_COLOR_ERROR = "red";
	private static final String TEXT_COLOR_NOTE = "gray";

	//A list of PIDs for external processes running as part of this algorithm.
	private List<Integer> PIDList = null;

	private StringBuffer sTextBuffer;

	private JLabel       	jTitle;
	private JButton      	jButtonDetails;
	private JButton      	jButtonCancel;
	private JCheckBox 		jCheckBoxClose = null;
	private JProgressBar 	jProgressBar;
	private JScrollPane  	jScrollPane = null;
	private JTextPane    	jTextPane = null;
	private JLabel       	jProgressText = null;
	private String       	m_sPrefix      = "";
	private String       	m_sDescription = "";
	private TableLayout		m_TableLayout = null;
	private AdjustmentListener scrollerAdjust = null;

	private boolean    		m_bCanceled = false;
	private boolean    		m_bDone = false;
	private boolean			m_bHasMessages = false;
	private boolean			m_bCloseWhenDone = true;
	private boolean 		m_bShowDetails = false;


	/**
	 * Constructor
	 *
	 * @param sText
	 *                The text to show
	 * @param bDeterminate
	 *                true if the process to monitor is determinate
	 * @param parent
	 *                the parent dialog (or main frame if passed as 'null')
	 * @param PID
	 *                a list of PIDs for being able to kill external processes (or 'null')                                
	 */
	public ProgressMonitor(final String sText, final boolean bDeterminate, final JDialog parent ) {
		super(parent, "", false);
		init(sText, bDeterminate);
	}


	/**
	 * Constructor. Uses the main frame as the parent component
	 *
	 * @param sText
	 *                The text to show
	 * @param bDeterminate
	 *                true if the process to monitor is determinate
	 */
	public ProgressMonitor(final String sText, final boolean bDeterminate ) {
		super(SextanteGUI.getMainFrame(), "", false);
		init(sText, bDeterminate);
	}

	private void init (final String sText, final boolean bDeterminate ) {
		m_bCanceled = false;
		m_bHasMessages = false;
		m_bCloseWhenDone = new Boolean(SextanteGUI.getSettingParameterValue
				(SextanteGeneralSettings.MONITOR_CLOSE_BY_DEFAULT)).booleanValue();
		m_bShowDetails = new Boolean(SextanteGUI.getSettingParameterValue
				(SextanteGeneralSettings.MONITOR_DETAILS_BY_DEFAULT)).booleanValue();

		m_sDescription = sText;

		PIDList = null;

		initGUI(sText, bDeterminate);
	}


	private void initGUI(final String sText, boolean bDeterminate)
	{
		setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		WindowListener exitListener = new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				if ( isDone() == true ) {
					/* Close window. */
					e.getWindow().setVisible(false);
				} else {
					/* Cancel current process. */
					cancel();
				}
			}
		};
		addWindowListener(exitListener);
		setLocationRelativeTo(null);
		
		m_TableLayout = new TableLayout(new double[][] {
				{ 6.0, TableLayoutConstants.FILL, TableLayoutConstants.FILL, TableLayoutConstants.FILL, 6.0 },
				{ 1.0, TableLayoutConstants.PREFERRED, 1.0, TableLayoutConstants.PREFERRED, 1.0, TableLayoutConstants.PREFERRED, 1.0 } });
		m_TableLayout.setHGap(5);
		m_TableLayout.setVGap(5);
		setPreferredSize(new Dimension(DIM_WIDTH_DEFAULT, DIM_HEIGHT_DEFAULT));
		getContentPane().setLayout(m_TableLayout);
		{
			jTitle = new JLabel(sText);
			getContentPane().add(jTitle, "1, 1, 3, 1");
		}				
		{
			jProgressBar = new JProgressBar();
			jProgressBar.setIndeterminate(!bDeterminate);
			if (bDeterminate) {
				jProgressBar.setMinimum(0);
				jProgressBar.setMaximum(100);
				jProgressBar.setValue(0);
				jProgressBar.setStringPainted(true);
			}
			getContentPane().add(jProgressBar, "1, 3, 3, 3");
		}
		{	
			jButtonCancel = new JButton();
			getContentPane().add(jButtonCancel, "3, 5");
			jButtonCancel.setText(Sextante.getText("Cancel"));
			jButtonCancel.addActionListener(new ActionListener() {
				public void actionPerformed(final ActionEvent evt) {
					cancel();
				}
			});
		}
		jScrollPane = null;
		jTextPane = null;
		jProgressText = null;

		sTextBuffer = new StringBuffer("");
	}


	private void addJButtonDetails () {
		if ( jButtonDetails == null ) {
			jButtonDetails = new JButton();			
			if ( m_bShowDetails == true ) {				
				jButtonDetails.setText(Sextante.getText("monitor_hide_details"));
			} else {
				jButtonDetails.setText(Sextante.getText("monitor_show_details"));
			}
			jButtonDetails.addActionListener(new ActionListener() {
				public void actionPerformed(final ActionEvent evt) {
					if ( m_bShowDetails == true ) {
						m_bShowDetails = false;
						jButtonDetails.setText(Sextante.getText("monitor_show_details"));
						//Remove row from layout.
						m_TableLayout.deleteRow(3);
						jScrollPane.setVisible(false);						
						if ( jProgressText != null ) {
							setPreferredSize(new Dimension(DIM_WIDTH_DEFAULT, DIM_HEIGHT_DEFAULT+DIM_HEIGHT_ROW_DEFAULT));
							setSize(new Dimension(DIM_WIDTH_DEFAULT, DIM_HEIGHT_DEFAULT+DIM_HEIGHT_ROW_DEFAULT));
						} else {
							setPreferredSize(new Dimension(DIM_WIDTH_DEFAULT, DIM_HEIGHT_DEFAULT));
							setSize(new Dimension(DIM_WIDTH_VERBOSE, DIM_HEIGHT_VERBOSE));
						}
						getContentPane().doLayout();
						pack();
					} else {
						m_bShowDetails = true;
						jButtonDetails.setText(Sextante.getText("monitor_hide_details"));
						if ( jProgressText != null ) {
							setPreferredSize(new Dimension(DIM_WIDTH_VERBOSE, DIM_HEIGHT_VERBOSE+DIM_HEIGHT_ROW_DEFAULT));
							setSize(new Dimension(DIM_WIDTH_VERBOSE, DIM_HEIGHT_VERBOSE+DIM_HEIGHT_ROW_DEFAULT));
						} else {
							setPreferredSize(new Dimension(DIM_WIDTH_VERBOSE, DIM_HEIGHT_VERBOSE));
							setSize(new Dimension(DIM_WIDTH_VERBOSE, DIM_HEIGHT_VERBOSE));
						}						
						//Add row to layout.
						m_TableLayout.insertRow(3, TableLayout.FILL);
						getContentPane().add(jScrollPane, "1, 3, 3, 3");
						jScrollPane.setVisible(true);
						getContentPane().doLayout();
						pack();
					}
					jButtonDetails.repaint();
				}
			});
			if ( jProgressText == null ) {
				getContentPane().add(jButtonDetails, "2, 5");
			} else {
				getContentPane().add(jButtonDetails, "2, 6");
			}
		} 
	}

	private void cancel() {

		if ( m_bCanceled ) {
			return;
		}
		jProgressBar.setString(Sextante.getText("sextante_progress_bar_label_cancelled"));
		jButtonCancel.setEnabled(false);
		if ( jCheckBoxClose != null ) {			
			jCheckBoxClose.setEnabled(m_bCloseWhenDone);
		}
		if ( jScrollPane != null ) {
			jScrollPane.getVerticalScrollBar().removeAdjustmentListener(scrollerAdjust);
		}		
		if ( jTextPane != null ) {
			sTextBuffer.append("<code style=\"color:"+ TEXT_COLOR_WARNING + "\">"+Sextante.getText("cancelling_process")+"</code><br>");
			jTextPane.setText(sTextBuffer.toString());
		}

		if ( PIDList != null ) {
			//Kill any external process(es)!
			if ( jTextPane != null ) {
				sTextBuffer.append("<code style=\"color:"+ TEXT_COLOR_WARNING + "\">"+Sextante.getText("killing_process")+"</code><br>");
				jTextPane.setText(sTextBuffer.toString());				
			}
			killProcessesByPID();
		}
		
		m_bCanceled = true;
	}


	/**
	 * Cancels the running process.
	 * But only if it is interruptible (it is by default).
	 */
	private void killProcessesByPID() {

		if ( PIDList == null ) return;

		String cmd = null;

		for ( int i = 0; i < PIDList.size(); i ++ ) {
			if ( Sextante.isWindows() ) {
				cmd = new String ("taskkill /f /t /pid " + PIDList.get(i).toString());
			}
			if ( Sextante.isUnix() || Sextante.isMacOSX() ) {
				cmd = new String ("kill -9 " + PIDList.get(i).toString());
			}
			try {
				Runtime.getRuntime().exec (cmd);
			} catch (Exception e) {
				//Could not kill the process!
				Sextante.addErrorToLog(e);
				JOptionPane.showMessageDialog(null, Sextante.getText("Warn_process_not_killed"),
						Sextante.getText("grass_error_title"), JOptionPane.WARNING_MESSAGE);
			}
		}

	}


	/**
	 * Returns true if the process has been canceled using the cancel button
	 *
	 * @return true if the process has been canceled
	 */
	public boolean isCanceled() {
		return m_bCanceled;		
	}


	/**
	 * Adds a line of status text output to the text area.
	 *
	 * @param sText
	 *                the message text
	 * @param sColor
	 *                the HTML text color
	 */
	private void setInfo(final String sText, final String sColor) {

		try {
			SwingUtilities.invokeAndWait(new Runnable() {
				public void run() {
					//First message? Create new text area in progress monitor!
					if ( jTextPane == null || jScrollPane == null ) {
						addJButtonDetails();
						jTextPane = new NonWordWrapPane();
						jTextPane.setStyledDocument(new DefaultStyledDocument());
						jTextPane.setContentType("text/html");
						jTextPane.setEditable(false);
						jScrollPane = new TransparentScrollPane();
						jScrollPane.setBackground(new Color(TEXT_COLOR_BACK[0],TEXT_COLOR_BACK[1],TEXT_COLOR_BACK[2]));
						jScrollPane.setSize(new java.awt.Dimension(DIM_WIDTH_VERBOSE, DIM_HEIGHT_VERBOSE));
						jScrollPane.setViewportView(jTextPane);
						//Add an adjustment listener that allows user to "scroll and halt"
						//during status message output.
						final BoundedRangeModel brm = jScrollPane.getVerticalScrollBar().getModel();
						jScrollPane.getVerticalScrollBar().addMouseWheelListener(new MouseWheelListener (){
							@Override
							public void mouseWheelMoved(MouseWheelEvent e) {
								brm.setValueIsAdjusting(true);
							}				
						});
						jScrollPane.addMouseWheelListener(new MouseWheelListener (){
							@Override
							public void mouseWheelMoved(MouseWheelEvent e) {
								brm.setValueIsAdjusting(true);
							}				
						});
						scrollerAdjust = new AdjustmentListener() {				
							boolean wasAtBottom = true;
							public void adjustmentValueChanged(AdjustmentEvent e) {
								if (!brm.getValueIsAdjusting()) {
									if (wasAtBottom)
										brm.setValue(brm.getMaximum());
								} else
									wasAtBottom = ((brm.getValue() + brm.getExtent()) == brm.getMaximum());
							}
						};
						jScrollPane.getVerticalScrollBar().addAdjustmentListener(scrollerAdjust);
						if ( m_bShowDetails == true ) {
							//Add row to layout.
							setPreferredSize(new Dimension(DIM_WIDTH_VERBOSE, DIM_HEIGHT_VERBOSE));
							setSize(new Dimension(DIM_WIDTH_VERBOSE, DIM_HEIGHT_VERBOSE));
							m_TableLayout.insertRow(3, TableLayout.FILL);
							getContentPane().add(jScrollPane, "1, 3, 3, 3");
						}

						//Add "Close when done" checkbox
						jCheckBoxClose = new JCheckBox(Sextante.getText("close_when_done"));
						jCheckBoxClose.setSelected(m_bCloseWhenDone);
						jCheckBoxClose.addActionListener(new ActionListener() {
							public void actionPerformed(final ActionEvent evt) {
								m_bCloseWhenDone = jCheckBoxClose.isSelected();
							}
						});
						if ( m_bShowDetails == true ) {
							getContentPane().add(jCheckBoxClose, "1, 7");
						} else {
							getContentPane().add(jCheckBoxClose, "1, 6");
						}

						getContentPane().doLayout();
					}				
					sTextBuffer.append("<code style=\"color:"+ sColor + "\">"+sText+"</code><br>");
					jTextPane.setText(sTextBuffer.toString());
					m_bHasMessages = true;
				}
			});
		}
		catch (final Exception e) {}
	}


	/**
	 * Adds a line of status text output to the text area.
	 *
	 * @param sText
	 *                the info message
	 */
	public void addInfo(final String sText) {
		setInfo ( sText, TEXT_COLOR_INFO );
	}


	/**
	 * Adds a line of error text output to the text area.
	 *
	 * @param sText
	 *                the error message
	 */
	public void addError(final String sText) {
		setInfo ( sText, TEXT_COLOR_ERROR );
	}


	/**
	 * Adds a line of warning text output to the text area.
	 *
	 * @param sText
	 *                the warning message
	 */
	public void addWarning(final String sText) {
		setInfo ( sText, TEXT_COLOR_WARNING );
	}


	/**
	 * Adds a line of warning text output to the text area.
	 *
	 * @param sText
	 *                the warning message
	 */
	public void addNote(final String sText) {
		setInfo ( sText, TEXT_COLOR_NOTE );
	}


	/**
	 * Sets the text describing the current phase of the process being monitored
	 * 
	 * @param sText
	 *                the description
	 */
	public void setProgressText(final String sText) {

		try {
			final Runnable runnable = new Runnable() {
				public void run() {
					if ( jProgressText == null ) {
						{
							jProgressText = new JLabel();
							Font labelFont = new Font(jProgressText.getFont().getName(),
									Font.ITALIC, jProgressText.getFont().getSize());
							jProgressText.setFont(labelFont);
							//Add row to layout.
							m_TableLayout.insertRow(2, TableLayout.PREFERRED);
							getContentPane().add(jProgressText, "1, 2, 3, 2");
							getContentPane().doLayout();
							if ( sText == null || sText.length() < 1 ) {
								setPreferredSize(new Dimension(DIM_WIDTH_DEFAULT, DIM_HEIGHT_DEFAULT));
								setSize(new Dimension(DIM_WIDTH_DEFAULT, DIM_HEIGHT_DEFAULT));
								pack();
							}														
						}						
					}
					if ( sText != null && sText.length() > 0 ) {
						jProgressText.setText(sText);
						if ( m_bShowDetails == true ) {
							setPreferredSize(new Dimension(DIM_WIDTH_VERBOSE, DIM_HEIGHT_VERBOSE+DIM_HEIGHT_ROW_DEFAULT));
							setSize(new Dimension(DIM_WIDTH_VERBOSE, DIM_HEIGHT_VERBOSE+DIM_HEIGHT_ROW_DEFAULT));
						} else {
							setPreferredSize(new Dimension(DIM_WIDTH_DEFAULT, DIM_HEIGHT_DEFAULT+DIM_HEIGHT_ROW_DEFAULT));
							setSize(new Dimension(DIM_WIDTH_DEFAULT, DIM_HEIGHT_DEFAULT+DIM_HEIGHT_ROW_DEFAULT));
						}
						pack();
					}					
				}
			};
			if (SwingUtilities.isEventDispatchThread()) {
				runnable.run();
			}
			else {
				SwingUtilities.invokeAndWait(runnable);
			}
		}
		catch (final Exception e) {}
	}


	/**
	 * Sets the description to show at the top of the progress monitor
	 *
	 * @param sDescription
	 *                the description
	 */
	public void setDescription(final String sDescription) {

		try {
			Runnable runnable = new Runnable() {
				public void run() {
					jTitle.setText(sDescription);
				}
			};
			if (SwingUtilities.isEventDispatchThread()){
				runnable.run();
			}
			else{
				SwingUtilities.invokeAndWait(runnable);
			}
		}
		catch (final Exception e) {}


	}


	private void updateTitle() {

		try {

			final Runnable runnable = new Runnable() {
				public void run() {
					jTitle.setText(m_sPrefix + m_sDescription);
				}
			};
			if (SwingUtilities.isEventDispatchThread()) {
				runnable.run();
			}
			else {
				SwingUtilities.invokeAndWait(runnable);
			}
		}
		catch (final Exception e) {}

	}


	/**
	 * this method sets a prefix to be prepended to the description title. This can be used for processes that include several
	 * algorithms, so when each algorithm sets its owns description, it will also contain a string indicating the part of the
	 * global process that it represents. This method should, therefore, not be called by simple algorithms, but just from complex
	 * processes
	 * 
	 * @param sPrefix
	 *                the prefix to prepend to the description title.
	 */
	public void setDescriptionPrefix(final String sPrefix) {

		m_sPrefix = sPrefix;
		updateTitle();

	}


	/**
	 * Sets the current progress value (in the range 1-100)
	 *
	 * @param iValue
	 */
	public void setProgress(final int iValue) {

		try {
			Runnable runnable = new Runnable() {
				public void run() {
					jProgressBar.setValue(iValue);
				}
			};
			if (SwingUtilities.isEventDispatchThread()){
				runnable.run();
			}
			else{
				SwingUtilities.invokeAndWait(runnable);
			}
		}
		catch (final Exception e) {}

	}


	/**
	 * Sets whether the process being monitored is determinate or not
	 *
	 * @param bDeterminate
	 */
	public void setDeterminate(final boolean bDeterminate) {

		try {
			Runnable runnable = new Runnable() {
				public void run() {
					jProgressBar.setIndeterminate(!bDeterminate);
				}
			};
			if (SwingUtilities.isEventDispatchThread()){
				runnable.run();
			}
			else{
				SwingUtilities.invokeAndWait(runnable);
			}
		}
		catch (final Exception e) {}

	}


	public boolean hasMessages() {
		return m_bHasMessages;
	}


	public boolean isCloseWhenDone() {
		return m_bCloseWhenDone;
	}


	public JButton getCancelCloseButton () {
		return (jButtonCancel);
	}


	public JCheckBox getCloseWhenDoneCheckbox () {
		return (jCheckBoxClose);
	}


	private class NonWordWrapPane
	extends
	JTextPane {
		private static final long serialVersionUID = 1L;


		public NonWordWrapPane() {

			super();

		}


		@Override
		public boolean getScrollableTracksViewportWidth() {

			return false;

		}

	}


	/**
	 * 
	 * Update the list of PIDs monitored by this ProgressMonitor.
	 * All processes on the list will be killed if the user presses
	 * "Cancel" on the monitor.
	 * 
	 * @param PIDs
	 * 			List of PIDs to kill if Cancel action is invoked (can be null)
	 */
	public void updatePIDList ( List<Integer> PIDs ) {
		PIDList = PIDs;
	}


	public boolean isDone() {
		return m_bDone;
	}


	/* To be set by DefaultTaskMonitor */ 
	public void setDone(boolean m_bDone) {
		this.m_bDone = m_bDone;
	}

}
