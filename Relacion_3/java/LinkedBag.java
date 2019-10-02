import java.util.Iterator;
import java.util.Spliterator;
import java.util.function.Consumer;

public class LinkedBag<T extends Comparable<? super T>> implements Bag <T>, Iterable<T> {

    static public class Node<E>{
        E elem;
        int count;
        Node<E> next;

        Node(E x, int n, Node<e> node){
            elem = x;
            count = n;
            next = node;
        }
    }

    static public class BagIterator<T extends Comparable<? super T>> implements Iterator<T> {
        private LinkedBag.Node<T> current;
        private LinkedBag<T> bag;

        public BagIterator(LinkedBag.Node firstNode, LinkedBag bag){
            current = firstNode;
            this.bag = bag;
        }

        public boolean hasNext(){
            return current != null;
        }

        @Override
        public T next(){
            Node<T> aux = current;
            current = current.next;
            return aux.elem;
        }

        @Override
        public void remove(){
            if(current.count > 1){
                current.count --;
            }else{
                Node<T> aux = current;
                current = current.next;
                bag.delete(aux.elem);
            }
        }

        @Override
        public void forEachRemaining(Consumer<? super T> action){
            throw new UnsupportedOperationException();
        }
    }

    private Node<T> first;

    public LinkedBag(){
        first = null;
    }

    public boolean isEmpty(){
        return this.first == null;
    }

    public void insert(T item){
        if(isEmpty()){
            first = new Node<>(item, 1, null);
        }else{
            Node<T> current, prev;
            current = first;
            prev = null;

            while(current != null && current.elem.compareTo(item) < 0){
                prev = current;
                current = current.next;
            }

            if(prev == null){ //Only one element
                prev = new Node<>(item, 1, current);
            }else if(current != null && current.elem.equals(item)){ //Repeated element
                current.count++;
            }else{ //Insert in correct position
                prev.next = new Node<>(item, 1, current);
            }
        }
    }

    public int occurrences(T item){
        int count = 0;
        if(!isEmpty()){
            Node<T> current = first;

            while (current != null && !current.elem.equals(item)){
                current = current.next;
            }

            if(current != null){
                count = current.count;
            }
        }
        return count;
    }

    public void delete(T item){
        if(!isEmpty()){
            Node<T> prev = null;
            Node<T> current = first;

            while(current != null && !current.elem.equals(item)){
                prev = current;
                current = current.next;
            }

            if(current != null && current.elem.equals(item)){
                current.count--;
                if(current.count < 1){
                    prev.next = current.next;
                }
            }
        }
    }

    public String toString(){
        String text = "Bag[";
        for(Node<T> p = first; p != null; p = p.next){
            text += "(" + p.elem + ", " + p.count + ")";
        }
        return text + "]";
    }

    @Override
    public Iterator<T> iterator(){
        return new BagIterator<T>(first, this);
    }

    @Override
    public void forEach(Consumer<? super T> action){
        throw new UnsupportedOperationException();
    }

    @Override
    public Spliterator<T> spliterator(){
        return null;
    }
}