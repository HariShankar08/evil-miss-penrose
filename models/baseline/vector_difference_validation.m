%% validate the vector difference model of semantic relations
% this code checks whether semantic relations defined by average vector 
% differences in word2vec semantic space are valid, i.e., whether the 
% vector difference of words that share a relation is more similar than
% that of words connected by other relations

clear
clc


%% data loading 

% load relational interpretations
load ..\baseline\relations.mat
relations = relations([1 2 3 4 7 8 9 10 13 16 19 22 25 26 29 30],:);

% number of instances over which computing the average relation vector
N_sents = 100;

% load semantic space from Baroni et al., (2014)
space = readtable("...\baroni.txt"); % path to semantic space
names = space{:,1};
vecs = space{:,2:end};
clear space



%% compute average vector differences

% change directory to load word-pairs linked by relations
directory = "...\baseline\relation_sentences\";
cd(directory);
opts = delimitedTextImportOptions("NumVariables", 4);
opts.VariableTypes = ["string", "string", "string", "string"];

vec_diffs = {};
vec_diffs_names = {};

for int = 1:height(relations)
    
    fprintf("%s%.0f%s%.0f\n","computing average vector of relation ",int,"/",height(relations))
    % ---------- (1) load data
    rel = strrep(join(relations(int,:))," ","_"); % set relation to work on
    % NOTE: due to lack of examples in UkWac, "location is" examples
    % pertain to sentences queries with "located in"

    l = readmatrix(sprintf("%s%s%s",directory,rel,"_left.csv"),opts); % left words are common nouns
    l(:,[1,3]) = [];
    r = readmatrix(sprintf("%s%s%s",directory,rel,"_right.csv"),opts); % right words are common nouns
    r(:,[1,3]) = [];

    % ---------- (2) get word pairs
    L = [];
    R = [];
    for i = 1:min(height(l),height(r))

        l1 = split(strip(l(i,1))); % left noun
        l1 = l1(end);
        l2 = split(strip(l(i,2)));
        l2 = l2(1);
        L = [L; l1 l2];

        r1 = split(strip(r(i,1))); % right noun
        r1 = r1(end);
        r2 = split(strip(r(i,2)));
        r2 = r2(1);
        R = [R; r1 r2];

    end
    S = L(ismember(join(L),join(R)),:);
    S = S(ismember(S(:,1),names) & ismember(S(:,2),names),:); % select word pairs available in the semantic space
    y = strip(unique(join(S)));
    y = y(~cellfun('isempty',strfind(strip(unique(join(S)))," "))); % exclude non word-pairs
    y = split(y);
    if (height(y) > N_sents)
        y = y(1:N_sents,:);
    end

    % ---------- (3) compute average vector difference
    V = [];
    n = [];
    for i = 1:height(y)
        if(relations(int,1) == "M" & relations(int,3) == "H")
            V = [V; vecs(names == y(i,1),:) - vecs(names == y(i,2),:)]; % direction from second (~M) to first word (~H)
        else
            V = [V; vecs(names == y(i,2),:) - vecs(names == y(i,1),:)]; % direction from first (~H) to second word (~M)
        end
        n = [n; y(i,:)];
    end
    vec_diffs{int} = V;
    vec_diffs_names{int} = n;
    
end



%% validate vector differences

rng(0,"twister");

names_diffs = [];
C = [];

for int = 1:height(relations)

    fprintf("%s%.0f%s%.0f\n","evaluating relation ",int,"/",height(relations))
    c = NaN(64,size(relations,1));
    y = vec_diffs_names{int};

    Ks = randperm(size(y,1));
    for kk = 1:64
        k = Ks(kk);

        % get vector difference of target word pair
        target_pair = y(k,:);
        v = vec_diffs{int}(k,:);

        % compute average vector difference of correct relation excluding pairs that share a constituent with the target pair
        Vs = [];
        for int2 = 1:height(relations)
            vv = vec_diffs{int2}(prod(not(ismember(vec_diffs_names{int2},target_pair(1))),2) & prod(not(ismember(vec_diffs_names{int2},target_pair(2))),2),:);
            Vs = [Vs; mean(vv)];
        end
        v_rel = Vs(int,:);
        Vs(int,:) = [];
        v_rel = [v_rel; Vs];

        % compare vector difference of target word pair with all average vector differences
        c(kk,:) = ((v_rel*v')./(sqrt(v*v')*sqrt(diag(v_rel*v_rel'))))'; % cosine with all average vector differences

    end

    c = sum(c(:,1) <= c,2); % rank of correct relation for each target pair
    C = [C; c'];
    names_diffs = [names_diffs; strjoin(relations(int,:))];

end

% statistical inference (left-tailed Wilcoxon signed-rank tests)
P = [];
for i = 1:16
    [p,h] = signrank(C(i,:),8.5,'tail','left');
    P = [P; p];
end



%% plot results

t = tiledlayout(4, 4, 'TileSpacing', 'Compact', 'Padding', 'Compact');

for i = 1:16
    nexttile
    histogram(C(i,:),'NumBins',16);
    xline(8.5,"red",'LineWidth',2);
    xline(median(C(i,:)),"black",'LineWidth',2);
    title(strrep(names_diffs(i),"_"," "),'FontSize', 8);
    xticks(1:5:17);
    xlim([0, 17]); 

    minYLimit = 15;
    currentYLim = ylim;
    ylim([0, max(currentYLim(2), minYLimit)]);
end
xlabel(t, 'rank of true relation');
ylabel(t, 'frequency');


