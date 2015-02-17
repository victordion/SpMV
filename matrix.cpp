// By Jianwei CUI
// Date of Creation: Feb 16 2015
// Matrix Vector Manipulation
#include <time.h>
#include <iostream>
#include <stdio.h>
#include <vector>
#include <exception>
#include <random>
#include <fstream>
#include <string>
#include <sstream>
#include <cmath>
#include <limits.h>
#include <algorithm>
using namespace std;

struct Node{
	Node():degree(0){
	}
	int degree;
	int id;
	vector<int> adjs;
};

struct NodeComparator {
	NodeComparator(vector<Node> & all_nodes) { this->_all_nodes = all_nodes; }
	bool operator () (const int & i, const int & j) {
		return _all_nodes[i].degree < _all_nodes[j].degree;
	}

	vector<Node> _all_nodes;
};



template<typename T>
class Vector{
private:
	T* elements;
	int size;
	const double fMin = 0.0;
	const double fMax = 1.0;
public:
	Vector(){
		size = 0;
		srand(0);
	}

	Vector(int _size){
		size = _size;
		srand(time(NULL));
	}


	void fillWithZeros(){
		for (int i = 0; i < size; i++){
			elements[i] = 0;
		}
	}
	void fillWithRandom(int _size){
		elements = new T[_size];
		size = _size;
		for (int i = 0; i < size; i++){
			double f = (double)rand() / RAND_MAX;
			T value = static_cast<T>(f);
			elements[i] = fMin + value * (fMax - fMin);
		}
	}

	void resize(int new_size){
		size = new_size;
		elements = new T[new_size];
	}

	int getSize(){
		return size;
	}

	Vector<T>& operator=(Vector<T> from){
		size = from.size;
		//delete [] elements;
		elements = new T[size];
		for (int i = 0; i < size; i++){
			elements[i] = from[i];
		}
		return *this;
	}

	T& operator[](int idx){
		if (idx > size){
			cout << idx << " " << size << endl;
			throw("Out of bound accessing vector");
		}
		else
			return elements[idx];
	}

	void displayContent(){
		for (int i = 0; i < size; i++){
			cout << elements[i] << endl;
		}
	}

	T getEuclideanDistance(Vector<T> vec){
		T distance = 0;
		if (size != vec.getSize())
			throw("getEuclideanDistance failed: vector size does not match.");
		else{
			for (int i = 0; i < size; i++){
				distance += (vec[i] - elements[i]) * (vec[i] - elements[i]);
			}
			distance = sqrt(distance / size);
		}

		return distance;
	}

};

template<typename T>
class Matrix{
private:
	T ** entries;
	int row_size;
	int column_size;
	int num_nonzeros;

	int * row_idx_coo;
	int * column_idx_coo;
	T * entries_coo;

public:

	Matrix(){
		num_nonzeros = 0;
		row_size = 0;
		column_size = 0;
	}

	~Matrix(){

	}

	int getRowSize(){ return row_size; }
	int getColumnSize(){ return column_size; }

	void showInfo(){
		cout << "Matrix Rows: " << row_size << endl;
		cout << "Matrix Columns: " << column_size << endl;
		cout << "Matrix Nonzeros: " << num_nonzeros << endl;
		for (int i = 0; i < num_nonzeros; i++){
			cout << "(" << row_idx_coo[i] << "," << column_idx_coo[i] << ") : " << entries_coo[i] << endl;
		}
	}

	void readFromFile(string file){
		ifstream infile(file);
		string line;
		bool unitialized = true;
		//int row_idx, column_idx;
		int idx_coo = 0;
//		T entry;
		while (getline(infile, line)){
			if (line.find("%") != std::string::npos){
				continue;
			}

			istringstream iss(line);
			if (unitialized == true){
				iss >> row_size >> column_size >> num_nonzeros;
				entries = new T*[row_size];
				for (int i = 0; i < row_size; i++){
					entries[i] = new T[column_size];
				}

				for (int i = 0; i < row_size; i++)
					for (int j = 0; j < column_size; j++)
						entries[i][j] = 0.0;

				row_idx_coo = new int[num_nonzeros];
				column_idx_coo = new int[num_nonzeros];
				entries_coo = new T[num_nonzeros];
				unitialized = false;
			}
			else{
				int r, c;
				T e;
				iss >> r >> c >> e;

				//cout << r << " " << c << " "<< e << endl;

				entries[r - 1][c - 1] = e;
				row_idx_coo[idx_coo] = r;
				column_idx_coo[idx_coo] = c;
				entries_coo[idx_coo] = e;
				++idx_coo;
				//cout << idx_coo << endl;
			}
		}
	}
	//template<typename T>
	Vector<T> multiplyVectorPlain(Vector<T> vec){
		Vector<T> ret;
		if (column_size != vec.getSize()){
			throw("matrix and vector size do not match!");
		}
		else{

			ret.resize(row_size);
			ret.fillWithZeros();
			for (int i = 0; i < row_size; i++){
				for (int j = 0; j < column_size; j++){
					//cout << i <<" " << j << endl;
					ret[i] = ret[i] + entries[i][j] * vec[j];
				}
			}
		}
		//cout << "Complete" << endl;
		return ret;
	}

	Vector<T> multiplyVectorCOO(Vector<T> vec){
		Vector<T> ret;
		if (getColumnSize() != vec.getSize())
			throw("matrix and vector size do not match!");
		else{

			ret.resize(row_size);
			ret.fillWithZeros();
			for (int k = 0; k < num_nonzeros; k++){
				int r = row_idx_coo[k];
				int c = column_idx_coo[k];
				T data = entries_coo[k];
				ret[r - 1] += data * vec[c - 1];
			}
		}
		return ret;
	}




	vector<int> findRCMOrdering(){
		vector<int> ret;

		// indexing from 1 instead of 0
		vector<Node> all_nodes(row_size + column_size + 1);
		bool* visited = new bool[row_size + column_size + 1];
		for (int i = 0; i < row_size + column_size + 1; i++){
			visited[i] = false;
		}

		for (int k = 0; k < num_nonzeros; k++){
			int n1 = row_idx_coo[k];
			int n2 = column_idx_coo[k] + row_size;
			all_nodes[n1].adjs.push_back(n2);
			all_nodes[n1].degree++;
			all_nodes[n2].adjs.push_back(n1); 
			all_nodes[n2].degree++;
		}

		int min_degree = INT_MAX;
		int min_degree_node_id = 1;
		for (int i = 1; i < row_size + column_size + 1; i++){
			if (all_nodes[i].degree < min_degree){
				min_degree = all_nodes[i].degree;
				min_degree_node_id = i;
			}
			if (min_degree == 0)
				break;
		}
		ret.push_back(min_degree_node_id);
		visited[min_degree_node_id] = true;

		vector<int> adj_nodes;

		while (ret.size() != row_size + column_size){

			//cout << "Just pushed Node " << ret.back() << endl;

			adj_nodes.clear();
			int curr_node_id = ret.back();
			bool pushed = false;
			for (int i = 0; i < all_nodes[curr_node_id].degree; i++){
				int temp = all_nodes[curr_node_id].adjs[i];
				if (visited[temp] == false){
					adj_nodes.push_back(all_nodes[curr_node_id].adjs[i]);
					pushed = true;
				}
			}
			if (pushed == true){
				//std::sort(adj_nodes.begin(), adj_nodes.end());
				std::sort(adj_nodes.begin(), adj_nodes.end(), NodeComparator(all_nodes));
				for (size_t i = 0; i < adj_nodes.size(); i++){
					ret.push_back(adj_nodes[i]);
					visited[adj_nodes[i]] = true;
				}
			}
			else{
				min_degree = INT_MAX;
				for (int i = 1; i < row_size + column_size + 1; i++){
					if (visited[i] == false && all_nodes[i].degree < min_degree ){
						min_degree = all_nodes[i].degree;
						min_degree_node_id = i;
					}
					if (min_degree == 0)
						break;
				}
				ret.push_back(min_degree_node_id);
				visited[min_degree_node_id] = true;
			}

		}
		return ret;
	}
};

int main(){


	Matrix<double> mtx;
	mtx.readFromFile("./matrices/rand5.mtx");
	//mtx.showInfo();

	Vector<double> vec;
	vec.fillWithRandom(mtx.getColumnSize());
	vec.displayContent();

	//Vector<double> result1;

	clock_t t1;
	clock_t t2;
	clock_t t3;
	clock_t t4;
	clock_t t5;
	clock_t t6;
	Vector<double> result1, result2;

	try{
		t1 = clock();
		result1 = mtx.multiplyVectorPlain(vec);
		t2 = clock();
		//result1.displayContent();
	}
	catch (char const * ex){
		cout << ex << endl;
	}

	try{
		t3 = clock();
		result2 = mtx.multiplyVectorCOO(vec);
		t4 = clock();
		//result1.displayContent();
	}
	catch (char const * ex){
		cout << ex << endl;
	}


	cout << "Distance is ";
	cout << result1.getEuclideanDistance(result2) << endl;

	t5 = clock();
	vector<int> ordering = mtx.findRCMOrdering();
	t6 = clock();
	for (size_t i = 0; i < ordering.size(); i++){
		cout << ordering[i] << endl;
	}
	cout << ordering.size() << endl;
	//result1.displayContent();
	//int a;
	//cin >>  a;
	//result2.displayContent();

	printf("%10.9f seconds elapsed.\n", (t2 - t1) / (double)CLOCKS_PER_SEC);
	printf("%10.9f seconds elapsed.\n", (t4 - t3) / (double)CLOCKS_PER_SEC);
	printf("%10.9f seconds elapsed.\n", (t6 - t5) / (double)CLOCKS_PER_SEC);
	printf("CLOCKS_PER_SEC = %10.9f.\n", (double)CLOCKS_PER_SEC);
	return 0;
}