import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.PrimitiveIterator.OfInt;
import java.util.stream.IntStream;

class Advent {
    public static void main(String[] args) throws IOException {
        IntStream jumpStream = Advent.fileToIntStream("input.txt");

        System.out.print("one: ");
        System.out.println(jumpStream.sum());

        HashSet<Integer> seen = new HashSet<Integer>();
        int[] jumps = Advent.fileToIntStream("input.txt").toArray();
        int jumpCount = jumps.length;
        int currentPosition = 0;
        int i = 0;

        while (true) {
            int currentJump = jumps[i++ % jumpCount];
            currentPosition += currentJump;

            if (seen.contains(currentPosition)) {
                break;
            }

            seen.add(currentPosition);
        }

        System.out.print("two: ");
        System.out.println(currentPosition);
    }

    private static IntStream fileToIntStream(String filename) throws IOException {
        return Files
            .lines(Paths.get(filename))
            .mapToInt(x -> Integer.parseInt(x));
    }
}
