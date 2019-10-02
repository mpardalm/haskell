public interface Bag <T extends Comparable<? super T>>{
    boolean isEmpty();
    void insert (T item);
    int occurrences (T item);
    void delete (T item);
}