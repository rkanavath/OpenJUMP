package es.unex.sextante.additionalInfo;

public class AdditionalInfoSelection
implements
AdditionalInfo {

	private final String[] m_sValues;
	private String m_sSelectionPath = "";


	public AdditionalInfoSelection(final String[] sValues) {

		m_sValues = sValues;
	}


	public String getSelectionPath() {

		return m_sSelectionPath;

	}


	public void setSelectionPath(final String sSelectionPath) {

		m_sSelectionPath = sSelectionPath;

	}   

	public String[] getValues() {

		return m_sValues;

	}


	public String getTextDescription() {
		// TODO Auto-generated method stub
		return null;
	}

}
