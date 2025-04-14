%% baseline model probabilities generation
% this code computes the truncated probabilites from:

% (i) the cosine similarities between vector differences computed from 
% constituents and from word pairs linked by a given semantic relation 
% (e.g., "made of") in a large corpora (UkWac); 
% (ii) the cosine similarities between vectors of relations and compound
% vectors computed by addition;
% (iii) the cosine similarities between vectors of relations and compound
% vectors computed with CAOSS;

% The code is applied to all data sources: self-paced reading for existing 
% and novel compounds (Benjamin et al., 2023) and existing compounds rated 
% in Schmidtke et al., (2018a,b), here labeled "decontextualized".



%% data loading and preprocessing

% load semantic space from Baroni et al., (2014)
space = readtable("...\baroni.txt"); % path to semantic space
names = space{:,1};
vecs = space{:,2:end};
clear space

% import CAOSS-derived vectors
opts = delimitedTextImportOptions("NumVariables", 401);
opts.DataLines = [1, Inf];
opts.Delimiter = "\t";
opts.VariableTypes = ["string", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double", "double"];
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";
caoss = readtable("...\CAOSS\COMPOSED_SS.FullAdditive.CAOSS_testset.txt.dm", opts);
clear opts

% load static semantic relations
diff_vecs = readtable("...\models\baseline\avg_relation_vecDiffs.csv");
stat_vecs = readtable("...\models\baseline\relation_vecs.csv");



%% generate baseline truncated probabilities: spr - existing

c = readtable("...\models\baseline\compNames_existing_spr.csv");
P = NaN(height(c),16);
P2 = NaN(height(c),16);
P3 = NaN(height(c),16);

for i = 1:height(c) 

    % (i) compute for relations as vector differences
    V = vecs(ismember(names,c{i,2}),:) - vecs(ismember(names,c{i,3}),:);
    p = 1 - squareform(pdist([V; diff_vecs{:,3:end}],'cosine'));
    p = p(1,2:end);
    P(i,:) = (p - min(p))/sum(p - min(p)); % compute truncated probabilities
    
    % (ii) compute for relations as static vectors from addition
    V = vecs(ismember(names,c{i,2}),:) + vecs(ismember(names,c{i,3}),:);
    p = 1 - squareform(pdist([V; stat_vecs{:,2:end}],'cosine')); 
    p = p(1,2:end);
    P2(i,:) = (p - min(p))/sum(p - min(p));
    
    % (iii) compute for relations as static vectors from CAOSS
    p = 1 - squareform(pdist([caoss{ismember(caoss{:,1},c{i,1}),2:end}; stat_vecs{:,2:end}],'cosine')); 
    p = p(1,2:end);
    P3(i,:) = (p - min(p))/sum(p - min(p));
    
end

P = array2table(P); % export (i)
P.Properties.VariableNames = diff_vecs{:,1};
T_sprEx = [c(:,1) P];
writetable(T_sprEx,"...\baseline_vecDiffs.csv");

P2 = array2table(P2); % export (ii)
P2.Properties.VariableNames = diff_vecs{:,1};
T_sprEx2 = [c(:,1) P2];
writetable(T_sprEx2,"...\baseline_vecs.csv");

P3 = array2table(P3); % export (iii)
P3.Properties.VariableNames = diff_vecs{:,1};
T_sprEx3 = [c(:,1) P3];
writetable(T_sprEx3,"...\baseline_CAOSS.csv");



%% generate baseline truncated probabilities: spr - novel

c = readtable("...\models\baseline\compNames_novel_spr.csv");
P = NaN(height(c),16);
P2 = NaN(height(c),16);
P3 = NaN(height(c),16);

% compute head-to-modifier vector difference
for i = 1:height(c) 

    % (i) compute for relations as vector differences
    V = vecs(ismember(names,c{i,2}),:) - vecs(ismember(names,c{i,3}),:);
    p = 1 - squareform(pdist([V; diff_vecs{:,3:end}],'cosine'));
    p = p(1,2:end);
    P(i,:) = (p - min(p))/sum(p - min(p));
    
    % (ii) compute for relations as static vectors from addition
    V = vecs(ismember(names,c{i,2}),:) + vecs(ismember(names,c{i,3}),:);
    p = 1 - squareform(pdist([V; stat_vecs{:,2:end}],'cosine')); 
    p = p(1,2:end);
    P2(i,:) = (p - min(p))/sum(p - min(p));
    
    % (iii) compute for relations as static vectors from CAOSS
    p = 1 - squareform(pdist([caoss{ismember(caoss{:,1},c{i,1}),2:end}; stat_vecs{:,2:end}],'cosine')); 
    p = p(1,2:end);
    P3(i,:) = (p - min(p))/sum(p - min(p));
    
end

P = array2table(P); % export (i)
P.Properties.VariableNames = diff_vecs{:,1};
T_sprNov = [c(:,1) P];
writetable(T_sprNov,"...\baseline_vecDiffs.csv");

P2 = array2table(P2); % export (ii)
P2.Properties.VariableNames = diff_vecs{:,1};
T_sprNov2 = [c(:,1) P2];
writetable(T_sprNov2,"...\baseline_vecs.csv");

P3 = array2table(P3); % export (iii)
P3.Properties.VariableNames = diff_vecs{:,1};
T_sprNov3 = [c(:,1) P3];
writetable(T_sprNov3,"...\baseline_CAOSS.csv");



%% generate baseline truncated probabilities: decontextualized

c = readtable('...\models\baseline\input.csv');
c = c(1:37:end,1:3); % get names of compounds decomposed into constituents
cosnames = readtable('...\models\baseline\compNames_decontextualized.xlsx');
c = c(ismember(c{:,1},cosnames{:,1}),:); % select only relevant compounds
P = NaN(height(c),16);
P2 = NaN(height(c),16);
P3 = NaN(height(c),16);

% compute head-to-modifier vector difference
for i = 1:height(c) 

    % (i) compute for relations as vector differences
    V = vecs(ismember(names,c{i,2}),:) - vecs(ismember(names,c{i,3}),:);
    p = 1 - squareform(pdist([V; diff_vecs{:,3:end}],'cosine'));
    p = p(1,2:end);
    P(i,:) = (p - min(p))/sum(p - min(p)); % compute truncated probabilities
    
    % (ii) compute for relations as static vectors from addition
    V = vecs(ismember(names,c{i,2}),:) + vecs(ismember(names,c{i,3}),:);
    p = 1 - squareform(pdist([V; stat_vecs{:,2:end}],'cosine')); 
    p = p(1,2:end);
    P2(i,:) = (p - min(p))/sum(p - min(p));
    
    % (iii) compute for relations as static vectors from CAOSS
    p = 1 - squareform(pdist([caoss{ismember(caoss{:,1},c{i,1}),2:end}; stat_vecs{:,2:end}],'cosine')); 
    p = p(1,2:end);
    P3(i,:) = (p - min(p))/sum(p - min(p));
    
end

P = array2table(P); % export (i)
P.Properties.VariableNames = diff_vecs{:,1};
T_decontextualized = [c(:,1) P];
writetable(T_decontextualized,"...\baseline_vecDiffs.csv");

P2 = array2table(P2); % export (ii)
P2.Properties.VariableNames = diff_vecs{:,1};
T_decontextualized2 = [c(:,1) P2];
writetable(T_decontextualized2,"...\baseline_vecs.csv");

P3 = array2table(P3); % export (iii)
P3.Properties.VariableNames = diff_vecs{:,1};
T_decontextualized3 = [c(:,1) P3];
writetable(T_decontextualized3,"...\baseline_CAOSS.csv");


