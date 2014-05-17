package com.henry4j;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Stack;
import java.util.function.Function;
import java.util.function.IntConsumer;
import java.util.function.IntPredicate;
import java.util.function.ObjIntConsumer;
import java.util.function.Predicate;
import java.util.stream.Stream;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.experimental.Accessors;

import org.hamcrest.collection.IsIterableContainingInAnyOrder;
import org.junit.Test;

import com.google.common.base.Joiner;
import com.google.common.primitives.Ints;

/*
http://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html
http://docs.oracle.com/javase/8/docs/api/java/util/stream/package-summary.html
http://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html
http://docs.oracle.com/javase/8/docs/api/java/util/stream/IntStream.html
http://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html
http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/CompletableFuture.html

http://www.journaldev.com/2389/java-8-features-for-developers-lambdas-functional-interface-stream-and-time-api
http://www.journaldev.com/2774/java-8-stream-api-example-tutorial
http://www.journaldev.com/2763/java-8-lambda-expressions-and-functional-interfaces-example-tutorial
http://leanpub.com/whatsnewinjava8/read
http://winterbe.com/posts/2014/03/16/java-8-tutorial/
*/
public class Jam {
    @Test
    public void test() {
        int[] ints = { 1, 2, 3, 4 };
        int[] even = Arrays.stream(ints).filter(i -> i % 2 == 0).toArray();
        System.out.println(Joiner.on(", ").join(Ints.asList(even)));
        Function<Double, Double> f = (Double d) -> d * d;
    }

    @Test
    public void testNavigate() {
        Edge[][] edges = new Edge[4][];
        edges[0] = new Edge[] { Edge.of(1), Edge.of(2), Edge.of(3) };
        edges[1] = new Edge[] { Edge.of(0), Edge.of(2), Edge.of(3) };
        edges[2] = new Edge[] { Edge.of(0), Edge.of(1), Edge.of(3) };
        edges[3] = new Edge[] { Edge.of(0), Edge.of(1), Edge.of(2) };
        List<int[]> paths = Graph.navigate(0, 3, edges);
        
//        assert_equal [[0, 1, 2, 3], [0, 1, 3], [0, 2, 3], [0, 3]], paths
//        assert_equal ["a→b→c→d", "a→b→d", "a→c→d", "a→d"], paths.map {|a| a.map { |e| ('a'[0] + e).chr }.join('→') }
//        assertThat(paths, equalTo(IsIterableContainingInAnyOrder.containsInAnyOrder(
//                new int[] {0, 1, 2, 3}, new int[] {0, 1, 3})));
    }

    @Test
    public void testTwoColorable() {
        // graph: B1 ― A0
        //        |    |
        //        C2 ― D3
        Edge[][] edges = new Edge[4][];
        edges[0] = new Edge[] { Edge.of(1), Edge.of(3) };
        edges[1] = new Edge[] { Edge.of(0), Edge.of(2) };
        edges[2] = new Edge[] { Edge.of(1), Edge.of(3) };
        edges[3] = new Edge[] { Edge.of(0), Edge.of(2) };
        assertThat(Graph.twoColorable(0, edges), equalTo(true));

        // graph: B1 ― A0
        //        |  X
        //        C2 ― D3
        edges = new Edge[4][];
        edges[0] = new Edge[] { Edge.of(1), Edge.of(2) }; // A0 - B1, A0 - C2
        edges[1] = new Edge[] { Edge.of(0), Edge.of(2), Edge.of(3) }; // B1 - A0, B1 - C2, B1 - D3
        edges[2] = new Edge[] { Edge.of(0), Edge.of(1), Edge.of(3) }; // C2 - A0, C2 - B1, C2 - D3
        edges[3] = new Edge[] { Edge.of(1), Edge.of(2) }; // D3 - B1, D3 - C2
        assertThat(Graph.twoColorable(0, edges), equalTo(false));
    }

    public static class Graph {
        public static List<int[]> navigate(int v, int w, Edge[][] edges) {
            List<int[]> paths = new ArrayList<>();
            boolean[] entered = new boolean[edges.length];
            Function<Stack<Integer>, Stream<Integer>> expandOut = a -> {
                entered[a.peek()] = true;
                return Stream.of(edges[a.peek()]).filter(e -> !entered[e.y()]).map(e -> e.y());
            };
            Predicate<Stack<Integer>> reduceOff = a -> {
                if (a.peek() == w) {
                    paths.add(Ints.toArray(a));
                    return true;
                } else {
                    return false;
                }
            };
            Stack<Integer> candidate = new Stack<>();
            candidate.push(v);
            Search.backtrack(candidate, expandOut, reduceOff);
            return paths;
        }

        public static boolean twoColorable(int v0, Edge[][] edges) {
            boolean[] bipartite = { true };
            boolean[] entered = new boolean[edges.length];
            Boolean[] colors = new Boolean[edges.length];
            IntPredicate enter_v_iff = v -> {
                if (bipartite[0] && !entered[v]) {
                    return entered[v] = true;
                } else {
                    return false;
                }
            };
            ObjIntConsumer<Edge> cross_e = (e, x) -> {
                bipartite[0] = bipartite[0] && colors[x] != colors[e.y()];
                colors[e.y()] = !colors[x];
            };
            for (int v = 0; v < edges.length; v++) {
                if (!entered[v]) {
                    Arrays.fill(entered, false);
                    Arrays.fill(colors, null);
                    colors[v] = true;
                    bfs(v, edges, enter_v_iff, null, cross_e);
                }
            }
            return bipartite[0];
        }

        public static void dfs(int v0, Edge[][] edges, IntPredicate enter_v_iff, IntConsumer exit_v, ObjIntConsumer<Edge> cross_e) {
            if (null == enter_v_iff || enter_v_iff.test(v0)) {
                Stream.of(edges[v0]).forEach(e -> {
                    if (cross_e != null) {
                        cross_e.accept(e, v0);
                    }
                    dfs(e.y(), edges, enter_v_iff, exit_v, cross_e);
                });
                if (null != exit_v) {
                    exit_v.accept(v0);
                }
             }
        }

        public static void bfs(int v0, Edge[][] edges, IntPredicate enter_v_iff, IntConsumer exit_v, ObjIntConsumer<Edge> cross_e) {
            Queue<Integer> q = new LinkedList<>();
            q.offer(v0);
            while (!q.isEmpty()) {
                final int v = q.poll();
                if (enter_v_iff == null || enter_v_iff.test(v)) {
                    Stream.of(edges[v]).forEach(e -> {
                        if (cross_e != null) {
                            cross_e.accept(e, v);
                        }
                        q.offer(e.y());
                    });
                    if (null != exit_v) {
                        exit_v.accept(v);
                    }
                }
            }
        }
    }

    @Data
    @Accessors(fluent = true)
    @AllArgsConstructor(staticName="of")
    @RequiredArgsConstructor(staticName="of")
    public static class Edge {
        private final int y;
        private int weight;
    }

    public static class Search {
        public static <E> void backtrack(
                Stack<E> candidate, Function<Stack<E>, Stream<E>> expandOut, Predicate<Stack<E>> reduceOff) {
            if (!reduceOff.test(candidate)) {
                expandOut.apply(candidate).forEach(e -> {
                    candidate.push(e);
                    backtrack(candidate, expandOut, reduceOff);
                    candidate.pop();
                });
            }
        }
    }
}