package it.geomaticaeambiente.klem;

public class InitialAbstraction {
    
    private InitialAbstraction() {}

    public InitialAbstraction(AbstractionUnits abstractionUnits, double abstractionValue) {
        this.abstractionUnits = abstractionUnits;
        this.abstractionValue = abstractionValue;
    }

    public AbstractionUnits getAbstractionUnits() {
        return abstractionUnits;
    }

    public double getAbstractionValue() {
        return abstractionValue;
    }

    private AbstractionUnits abstractionUnits;
    private double abstractionValue;
        
    public enum AbstractionUnits {
        FRACTION, MILLIMETERS;
    }
    
}
