package com.henry4j;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Queue;
import java.util.function.Function;
import java.util.function.IntConsumer;
import java.util.function.IntPredicate;
import java.util.function.ObjIntConsumer;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.experimental.Accessors;

import org.junit.Test;

import com.google.common.base.Joiner;
import com.google.common.primitives.Ints;

public class AppTest {
    @Test
    public void test() {
        int[] ints = { 1, 2, 3, 4 };
        int[] even = Arrays.stream(ints).filter(i -> i % 2 == 0).toArray();
        System.out.println(Joiner.on(", ").join(Ints.asList(even)));
        Function<Double, Double> f = (Double d) -> d * d;
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

//        # graph: B1 ― A0
//        #        |  X
//        #        C2 ― D3
//        edges = []
//        edges << [Edge.new(1), Edge.new(2)] # A0 - B1, A0 - C2
//        edges << [Edge.new(0), Edge.new(2), Edge.new(3)] # B1 - A0, B1 - C2, B1 - D3
//        edges << [Edge.new(0), Edge.new(1), Edge.new(3)] # C2 - A0, C2 - B1, C2 - D3
//        edges << [Edge.new(1), Edge.new(2)] # D3 - B1, D3 - C2
//        assert !Graph.two_colorable?(0, edges)
    }

    @Data
    @Accessors(fluent = true)
    @AllArgsConstructor(staticName="of")
    @RequiredArgsConstructor(staticName="of")
    public static class Edge {
        private final int y;
        private int weight;
    }

    public static class Graph {
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
                    colors[v] = true;
                    bfs(v, edges, enter_v_iff, null, cross_e);
                    Arrays.fill(colors, null);
                }
            }
            return bipartite[0];
        }
    }

    public static void bfs(int v0, Edge[][] edges, IntPredicate enter_v_iff, IntConsumer exit_v, ObjIntConsumer<Edge> cross_e) {
        Queue<Integer> q = new ArrayDeque<>();
        q.offer(v0);
        while (!q.isEmpty()) {
            int[] v = { q.poll() };
            if (enter_v_iff == null || enter_v_iff.test(v[0])) {
                Arrays.stream(edges[v[0]]).forEach(e -> {
                    if (cross_e != null) {
                        cross_e.accept(e, v[0]);
                    }
                    q.offer(e.y());
                });
                if (null != exit_v) {
                    exit_v.accept(v[0]);
                }
            }
        }
    }
}