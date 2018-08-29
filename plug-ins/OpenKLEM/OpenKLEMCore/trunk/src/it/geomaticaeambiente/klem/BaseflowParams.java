package it.geomaticaeambiente.klem;

public class BaseflowParams {
        
    private BaseflowParams() {}

    public BaseflowParams(BaseflowType type, Double initialBaseflow, double recession) {
        this.type = type;
        this.initialBaseflow = initialBaseflow;
        this.recession = recession;
    }        

    public BaseflowType getBaseflowType() {
        return type;
    }
    
    public void setBaseflowType(BaseflowType baseflowType) {
        this.type = baseflowType;
    }
    
    public Double getInitialBaseflow() {
        return initialBaseflow;
    }

    public void setInitialBaseflow(Double initialBaseflow) {
        this.initialBaseflow = initialBaseflow;
    }
    
    public double getRecession() {
        return recession;
    }

    public void setRecession(double recession) {
        this.recession = recession;
    }

    public enum BaseflowType {
        LUMPED, DISTRIBUTED;
    }
    
    private BaseflowType type;
    private Double initialBaseflow;
    private double recession;
    
}
