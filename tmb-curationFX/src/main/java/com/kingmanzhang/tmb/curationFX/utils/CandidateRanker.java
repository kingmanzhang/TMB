package com.kingmanzhang.tmb.curationFX.utils;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class CandidateRanker {

    public static List<String> ordered(String query, List<String> candidates){

        candidates.sort(Comparator.comparingInt(o -> score(query, o)));
        Collections.reverse(candidates);
        return candidates;
    }

    public static int score(String query, String candidate){
        query = query.toLowerCase();
        candidate = candidate.toLowerCase();
        if (query.equals(candidate)){
            return 100;
        }
        int score = 0;
        String[] query_words = query.split("\\W");
        for (int i = 0; i < query_words.length; i++){
            if (candidate.contains(query_words[i])){
                score += 1;
            }
        }
        return score;
    }

}
