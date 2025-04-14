%% static relations computations
% this code generates vector representations for compound word relational
% interpretations from word2vec semantic space from Baroni et al., (2014).
% Two methods:
% (i) semantic relations as average vector differences
% (ii) semantic relations described by the relations' word vectors



%% data loading 

% load relational interpretations
load ..\models\baseline\relations.mat
relations = relations([1 2 3 4 7 8 9 10 13 16 19 22 25 26 29 30],:);

% number of instances over which computing the average relation vector
N_sents = 100;

% load semantic space from Baroni et al., (2014)
space = readtable("...\baroni.txt"); % path to semantic space
names = space{:,1};
vecs = space{:,2:end};
clear space



%% computing vector differences

% change directory to load word-pairs linked by relations
directory = "...\models\baseline\relation_sentences\";
cd(directory);
opts = delimitedTextImportOptions("NumVariables", 4);
opts.VariableTypes = ["string", "string", "string", "string"];

vec_diffs = [];
names_diffs = [];
N_cases = [];

for int = 1:height(relations)
    
    % ---------- (1) load data
    rel = strrep(join(relations(int,:))," ","_"); % set relation to work on
    % NOTE: due to lack of examples in UkWac, "location is" examples
    % pertain to sentences queries with "located in"
    fprintf("%s%s\n","computing: ",rel);

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
    for i = 1:height(y)
        V = [V; vecs(names == y(i,2),:) - vecs(names == y(i,1),:)]; % direction from first (~H) to second word (~M)
    end
    vec_diffs = [vec_diffs; mean(V)];
    names_diffs = [names_diffs; rel];
    N_cases = [N_cases; height(y)];
    
end

% reverse vector for reversed relations
vec_diffs = vec_diffs .* (-1*double(relations(:,1) == "M") + double(relations(:,1) == "H"));

% export
names_diffs = array2table(names_diffs);
names_diffs.Properties.VariableNames{1} = 'relation';
N_cases = array2table(N_cases);
N_cases.Properties.VariableNames{1} = 'instances';
T = [names_diffs N_cases array2table(vec_diffs)];
%writetable(T,"...\models\baseline\avg_relation_vecDiffs.csv");



%% Semantic relations from the relations word vectors
% get actual vectors for the 16 relations from the semantic space

% "for" is in position 10 (check manually)
names{10,1} = 'for';

Vs = [];
for i = 1:height(relations)
    a = split(relations(i,2));
    if (height(a) == 1) % if relation is multiword, take average embedding
        Vs = [Vs; vecs(ismember(names,a),:)];
    else
        Vs = [Vs; (vecs(ismember(names,a(1)),:) + vecs(ismember(names,a(2)),:))/2];
    end
end

% export
n = array2table(strrep(join(relations)," ","_"));
n.Properties.VariableNames{1} = 'relation';
T = [n array2table(Vs)];
%writetable(T,"...\models\baseline\relation_vecs.csv");


