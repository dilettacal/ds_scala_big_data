package assignment1;

public class CoinsCount {

    public static int[] changes = {1,2};

    public static int countChanges(int amount, int currentCoint){
        if (amount == 0) return 1;
        if (amount < 0) return 0;
        int nCombos = 0;
        for(int coin = 0; coin < changes.length; coin++){
            nCombos+= countChanges(amount - changes[coin], coin);
        }
        return nCombos;

    }

    public static void main(String[] args) {
        System.out.println(countChanges(4, 0));
    }
}
