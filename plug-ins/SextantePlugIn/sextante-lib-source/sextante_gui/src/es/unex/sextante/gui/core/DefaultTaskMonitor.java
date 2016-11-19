package es.unex.sextante.gui.core;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.SwingUtilities;

import es.unex.sextante.core.ITaskMonitor;
import es.unex.sextante.core.Sextante;

/**
 * A simple task monitor, which just shows a progress monitor
 * 
 * @author volaya
 * 
 */
public class DefaultTaskMonitor
         implements
            ITaskMonitor {

   private ProgressMonitor m_ProgressMonitor;


   /**
    * Creates a new task monitor.
    * 
    * @param sText
    *                the text to show in the top bar of the progress monitor
    * @param bDeterminate
    *                whether the process to monitor is determinate (the number of total steps to complete is known)
    * @param parent
    *                the parent component. If null, the main frame will be used
    */
   public DefaultTaskMonitor(final String sText,
                             final boolean bDeterminate,
                             final JDialog parent) {

	   init ( sText, bDeterminate, parent );

   }


   /**
    * 
    * Initializes the monitor with the arguments passed to the constructor.
    * 
    * @param sText
    * @param bDeterminate
    * @param parent
    * @param PID
    * 		A list of PIDs of external processes. May be 'null'.
    */
   private void init (final String sText,
           final boolean bDeterminate,
           final JDialog parent ) {
	   try {

	         final Runnable runnable = new Runnable() {
	            public void run() {
	               if (parent != null) {
	                  m_ProgressMonitor = new ProgressMonitor(sText, bDeterminate, parent );
	               }
	               else {
	                  m_ProgressMonitor = new ProgressMonitor(sText, bDeterminate );
	               }
	               m_ProgressMonitor.pack();
	               m_ProgressMonitor.setVisible(true);
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
  
  
   public boolean isCanceled() {

      if (m_ProgressMonitor != null) {
         return m_ProgressMonitor.isCanceled();
      }
      else {
         return true;
      }

   }


   public void setProgress(final int iStep) {

      if (m_ProgressMonitor != null) {
         m_ProgressMonitor.setProgress(iStep);
      }

   }


   public void setProgressText(final String sText) {

      if (m_ProgressMonitor != null) {
         m_ProgressMonitor.setProgressText(sText);
      }

   }


   public void close() {
	   if (m_ProgressMonitor != null ) {
		   m_ProgressMonitor.setDone(true);
		   if ( m_ProgressMonitor.isCloseWhenDone() || m_ProgressMonitor.hasMessages() == false ) {
			   m_ProgressMonitor.setVisible(false);
			   m_ProgressMonitor.dispose();
		   } else {
			   JCheckBox closeWhenDone = m_ProgressMonitor.getCloseWhenDoneCheckbox();
			   closeWhenDone.setEnabled(false);
			   JButton close = m_ProgressMonitor.getCancelCloseButton();
			   close.setEnabled(true);
			   close.setText(Sextante.getText("Close"));
			   close.addActionListener(new ActionListener() {
				   public void actionPerformed(final ActionEvent evt) {
					   m_ProgressMonitor.setVisible(false);
					   m_ProgressMonitor.dispose();
				   }
			   });
    	 }
      }

   }


   public void setProgress(final int iStep,
                           final int iTotalNumberOfSteps) {

      if (m_ProgressMonitor != null) {
         if (iTotalNumberOfSteps != 0) {
            double dPercent = (double) iStep / (double) iTotalNumberOfSteps;
            dPercent = Math.min(Math.max(0, dPercent), 1);
            final int iCurrentStep = Math.min((int) (dPercent * 100), 100);
            m_ProgressMonitor.setProgress(iCurrentStep);
         }
      }


   }
   
   
   /*
    * Notifies the attached (if any) ProgressMonitor that its PID list
    * needs to be updated.
    */
   public void setPIDList ( List<Integer> PIDs) {
	   if (m_ProgressMonitor != null) {
		   m_ProgressMonitor.updatePIDList(PIDs);
	   }
   }


   public void setDeterminate(final boolean bDeterminate) {

      if (m_ProgressMonitor != null) {
         m_ProgressMonitor.setDeterminate(bDeterminate);
      }

   }


   public void setProcessDescription(final String sDescription) {

      if (m_ProgressMonitor != null) {
         m_ProgressMonitor.setDescription(sDescription);
      }

   }


   public void setDescriptionPrefix(final String sPrefix) {

      if (m_ProgressMonitor != null) {
         m_ProgressMonitor.setDescriptionPrefix(sPrefix);
      }

   }
   
   
   public void showInfo (final String msg) {

	   if (m_ProgressMonitor != null) {
	         m_ProgressMonitor.addInfo(msg);
	      }
   }
   
   public void showWarning (final String msg) {

	   if (m_ProgressMonitor != null) {
	         m_ProgressMonitor.addWarning(msg);
	      }
   }
   
   public void showError (final String msg) {

	   if (m_ProgressMonitor != null) {
	         m_ProgressMonitor.addError(msg);
	      }
   }
   
   public void showNote (final String msg) {

	   if (m_ProgressMonitor != null) {
	         m_ProgressMonitor.addNote(msg);
	      }
   }

}
