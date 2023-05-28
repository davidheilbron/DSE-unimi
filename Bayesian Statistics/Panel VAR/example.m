%% Panel BVAR
close all; clc; clear;

cd 'C:\Users\Administrator\Documents\MATLAB\Panel BVAR';
pvardata = readtable('pvar_data.xslx');

startTime = datetime('2018-01-01');  % Specify the start time
endTime = datetime('2020-01-01');    % Specify the end time

% In-sample data
y = pvardata(pvardata.Time >= startTime & pvardata.Time <= endTime,:);


