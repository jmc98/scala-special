package coursera;

/**
 * Created by Michael on 12/05/2016.
 */
public class JTest
{
    enum Thing {
        ONE,
        TWO,
        THREE {
            @Override
            int getVal() {
                return 2;
            }
        }
        ;

        int getVal() { return 5; }
    }

    public static void main( String[] args )
    {
        System.out.println( "ONE = " + Thing.ONE.getClass() + " ; TWO = " + Thing.TWO.getClass() + " ; THREE = " + Thing.THREE.getClass() );
    }
}
