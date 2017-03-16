#include<iostream>
using namespace std;
#include<string>
#include<math.h>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>

struct color{
	int x;			//first coordinate
	int y;			//second coordinate
	int z;			//third coordinate
	string c;		//the color name
};

float euqDist(float, float, float, float, float, float);		//euclidian distance between two colors.
float simOneColor(float, float);									//similarity between two instances of the same color.
float simByED(float);												//similarity degree of two colors via euqlidian distance.
float simByAlg(float, float, float, float, float, float);	//similarity degree of two colors via our algorithm.
float credBySim(float, float);									//credibility value defined from proximity of similarity values.
float weightEval(float, float= 1, float = (1/3.0));				//weight w, evaluated from cred function.	
float credEval(float, float= 1, float = (1/3.0));				//final credibility value, computed via global weight
void inputHandle(ifstream& f,  vector<vector<float>> &c, string fileName);
void inputColor(ifstream& f,  vector<string> &col, string fileName);
void inputComplete(ifstream& f,  vector<color> &c, string fileName);

int main()
{
	string pc;
	begin:
	cout<<"Please select your choice of program: ";
	cin>>pc;

	if(pc == "1" || pc == "A" || pc == "a"){
		ifstream f1, f2;
		vector<vector<float>> colorPairs;
		inputHandle(f1, colorPairs, "colors.txt");
		vector<string> colorNames;
		inputColor(f2, colorNames, "colorNames.txt");

		cout<<"\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		cout<<"~~~~~~~~~~~~~~~~~~~~TRAINING SET~~~~~~~~~~~~~~~~~~~~\n";
		cout<<"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

		float weightMin = 1;
		for(int i = 0; i < colorPairs.size() - 2; i++){
				float sim1 = simByAlg(colorPairs[i][0], colorPairs[i][1], colorPairs[i][2], colorPairs[i][3], colorPairs[i][4], colorPairs[i][5]);
				float sim2 = simByED(euqDist(colorPairs[i][0], colorPairs[i][1], colorPairs[i][2], colorPairs[i][3], colorPairs[i][4], colorPairs[i][5]));
				float cred2 = credBySim(sim1, sim2);
				float weight = weightEval(cred2);
				if(weight < weightMin)
					weightMin = weight;

				cout<<"Color_pair["<<i<<"], "<<colorNames[i*2]<<" and "<<colorNames[i*2+1]<<": "<<endl;
				cout<<"_________________________________________"<<endl;
				cout<<"sim1:\t"<<sim1<<"\tsim2:\t"<<sim2<<"\ncred2:\t"<<cred2<<" \tweight:\t"<<weight<<"\n\n\n";
		}

		cout<<"************************************************\n";
		cout<<"\tGlobal weight: "<<weightMin<<endl;
		cout<<"************************************************\n\n\n";

		cout<<"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		cout<<"~~~~~~~~~~~~~~~~~~~~~~TEST SET~~~~~~~~~~~~~~~~~~~~~~\n";
		cout<<"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

		int s = colorPairs.size();
		for(int t = s -2; t < s; t++){
			float sim1 = simByAlg(colorPairs[t][0], colorPairs[t][1], colorPairs[t][2], colorPairs[t][3], colorPairs[t][4], colorPairs[t][5]);
			float sim2 = simByED(euqDist(colorPairs[t][0], colorPairs[t][1], colorPairs[t][2], colorPairs[t][3], colorPairs[t][4], colorPairs[t][5]));
			cout<<"Color_pair["<<t<<"], "<<colorNames[t*2]<<" and "<<colorNames[t*2+1]<<": "<<endl;
			cout<<"_________________________________________"<<endl;
			cout<<"sim1:\t"<<sim1<<" \tsim2:\t"<<sim2<<"\n\n";

			float cred1 = credEval(weightMin);	//global weight is used for finding the final credibility value
			cout<<"cred1:\t"<<cred1<<endl;
			float simFin = cred1 * sim1;
			cout<<"simFin:\t"<<simFin;

			cout<<"\n\n************************************************\n";
			cout<<"\t"<<simFin<<"     =<     "<<sim2<<endl;
			cout<<"************************************************\n\n\n";
		}
	}

	else if(pc == "2" || pc == "B" || pc == "b"){
		ifstream f3;
		vector<color> col;
		inputComplete(f3, col, "colorsAll.txt");
		string userColor1, userColor2;

		cout<<"\nPlease choose two colors: ";
		cin>>userColor1;
		cin>>userColor2;

		color temp1, temp2;
		for(int i = 0; i < col.size(); i++){
			if(col[i].c == userColor1){
				temp1.c = col[i].c;				//store the color at a temp variable
				temp1.x = col[i].x;
				temp1.y = col[i].y;
				temp1.z = col[i].z;

				col[i].c = col[col.size()-1].c;	//switch it with the last element of
				col[i].x = col[col.size()-1].x;	//the vector in order to pop it out
				col[i].y = col[col.size()-1].y;	//of the list
				col[i].z = col[col.size()-1].z;
				col.pop_back();
			}
		}

		for(int i = 0; i < col.size(); i++){
			if(col[i].c == userColor2){
				temp2.c = col[i].c;				//store the color at a temp variable
				temp2.x = col[i].x;
				temp2.y = col[i].y;
				temp2.z = col[i].z;

				col[i].c = col[col.size()-1].c;	//switch it with the last element of
				col[i].x = col[col.size()-1].x;	//the vector in order to pop it out
				col[i].y = col[col.size()-1].y;	//of the list
				col[i].z = col[col.size()-1].z;
				col.pop_back();
			}
		}

		cout<<"\n Weights evaluated via Min or Average rule: ";
		cin>>pc;

		float weightGlo = 1.0;		//global weight
		if(pc == "1" || pc == "m" || pc == "M"){
			color t1, t2;
			int pn =1;					//pair counter

			for(int i = 0; i < col.size(); i++){
				for(int j = i+1; j < col.size(); j++){
					float sim1 = simByAlg(col[i].x, col[i].y, col[i].z, col[j].x, col[j].y, col[j].z);
					float sim2 = simByED(euqDist(col[i].x, col[i].y, col[i].z, col[j].x, col[j].y, col[j].z));
					float cred2 = credBySim(sim1, sim2);
					float weight = weightEval(cred2);
					if(weight < weightGlo)
						weightGlo = weight;

					cout<<"Color_pair["<<pn<<"], "<<col[i].c<<" and "<<col[j].c<<": "<<endl;
					cout<<"_________________________________________"<<endl;
					cout<<"sim1:\t"<<sim1<<"\tsim2:\t"<<sim2<<"\ncred2:\t"<<cred2<<" \tweight:\t"<<weight<<"\n\n\n";
					pn++;
				}
			}
		}

		else if(pc == "a" || pc == "A" || pc == "2"){
			color t1, t2;
			vector<float> weights;
			int pn =1;					//pair counter
			for(int i = 0; i < col.size(); i++){
				for(int j = i+1; j < col.size(); j++){
					float sim1 = simByAlg(col[i].x, col[i].y, col[i].z, col[j].x, col[j].y, col[j].z);
					float sim2 = simByED(euqDist(col[i].x, col[i].y, col[i].z, col[j].x, col[j].y, col[j].z));
					float cred2 = credBySim(sim1, sim2);
					float weight = weightEval(cred2);
					weights.push_back(weight);

					cout<<"Color_pair["<<pn<<"], "<<col[i].c<<" and "<<col[j].c<<": "<<endl;
					cout<<"_________________________________________"<<endl;
					cout<<"sim1:\t"<<sim1<<"\tsim2:\t"<<sim2<<"\ncred2:\t"<<cred2<<" \tweight:\t"<<weight<<"\n\n\n";
					pn++;
				}
			}
			for(int i = 0; i < col.size(); i++)
				weightGlo += weights[i];
			weightGlo /= col.size();
		}

		cout<<"************************************************\n";
		cout<<"\tGlobal weight: "<<weightGlo<<endl;
		cout<<"************************************************\n\n\n";

		cout<<"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		cout<<"~~~~~~~~~~~~~~~~~~~~~~TEST SET~~~~~~~~~~~~~~~~~~~~~~\n";
		cout<<"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";

		float sim1 = simByAlg(temp1.x, temp1.y, temp1.z, temp2.x, temp2.y, temp2.z);
		float sim2 = simByED(euqDist(temp1.x, temp1.y, temp1.z, temp2.x, temp2.y, temp2.z));
		cout<<"Color_pair[focus], "<<temp1.c<<" and "<<temp2.c<<": "<<endl;
		cout<<"_________________________________________"<<endl;
		cout<<"sim1:\t"<<sim1<<" \tsim2:\t"<<sim2<<"\n\n";

		float cred1 = credEval(weightGlo);	//global weight is used for finding the final credibility value
		cout<<"cred1:\t"<<cred1<<endl;
		float simFin = cred1 * sim1;
		cout<<"simFin:\t"<<simFin;

		cout<<"\n\n************************************************\n";
		cout<<"\t"<<simFin<<"     =<     "<<sim2<<endl;
		cout<<"************************************************\n\n\n";
	}

	else{
		cout<<"Wrong command!\n";
		goto begin;
	}

	getchar();
	getchar();
	return 0;
}

//euclidian distance between two colors
float euqDist(float a, float b, float c, float x, float y, float z)
{
	return sqrt(pow(fabs(a-x),2)+pow(fabs(b-y),2)+pow(fabs(c-z),2));
}

//similarity between two instances of the same color. (ex: red_90 and red_187)
float simOneColor(float c1, float c2)
{
	return 1-((fabs(c1-c2))/255);
}

//similarity degree of two colors via euqlidian distance
float simByED(float dist)
{
	return 1-(dist/(255*sqrt(3.0)));
	/*
	float res = 1-(dist/(255*sqrt(3.0)));
	if(res < 0.1)
		return 0;
	else
		return res;*/
}

//similarity degree of two colors via our algorithm
float simByAlg(float a, float b, float c, float x, float y, float z)
{
	return (((simOneColor(a, x))+(simOneColor(b, y))+(simOneColor(c, z)))/3);
}

//credibility value defined from proximity of similarity values
float credBySim(float sim1, float sim2)
{
	//return 1-(fabs(sim1 - sim2));
	float sim = (sim2+0.01) / (sim1+0.01); 	//in order to prevent division by zero
	if(sim > 1)
		return 1;
	else 
		return sim;
}

//weight w, evaluated from cred function
float weightEval(float cred, float va, float ea)				//default values for va and ea are 1 and 1/3 respectively
{																	//since those are the most common cases, but could be overwritten
	return ((cred-ea)/(va-ea));
}

float credEval(float weight, float va, float ea)				//again default values for va and ea are 1 and 1/3 respectively
{
	return ((weight * va) + ((1-weight) * ea));
}


void inputHandle(ifstream& f,  vector<vector<float>> &c, string fileName)
{
	string temp;
	f.open(fileName);

	if (!f) {
	cout << "Unable to open file";
	cin>>temp;
	exit(1); // terminate with error
	}

	int i = 0;
	char line[1024];


	while(f.getline(line, 1024)){
		istringstream ss(line, istringstream::in);
		float rate;
		vector<float> temp;

		c.push_back(temp);

		for(int j = 0; j < 6; j++){
			ss >> rate;
			c[i].push_back(rate);
		}
		i++;
	}

	f.close();
}

void inputColor(ifstream& f,  vector<string> &c, string fileName)
{
	string temp;
	f.open(fileName);

	if (!f) {
	cout << "Unable to open file";
	cin>>temp;
	exit(1); // terminate with error
	}

	char line[1024];
	string tempColor;

	while(f.getline(line, 1024)){
		istringstream ss(line, istringstream::in);
		ss >> tempColor;				//first color of the pairing
		c.push_back(tempColor);
		ss >> tempColor;				//second color of the pairing
		c.push_back(tempColor);
	}

	f.close();
}

void inputComplete(ifstream& f,  vector<color> &c, string fileName)
{
	string temp;
	f.open(fileName);

	if (!f) {
	cout << "Unable to open file";
	cin>>temp;
	exit(1); // terminate with error
	}

	char line[1024];

	while(f.getline(line, 1024)){
		istringstream ss(line, istringstream::in);
		color temp;

		ss >> temp.c;
		ss >> temp.x;
		ss >> temp.y;
		ss >> temp.z;

		c.push_back(temp);

		cout<<temp.x<<" \t"<<temp.y<<" \t"<<temp.z<<" \t"<<temp.c<<endl;
	}

	f.close();
}