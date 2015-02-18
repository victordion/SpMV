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
#include <queue>
using namespace std;

struct Node{
	Node():degree(0){
	}
	int degree;
	int id;
	vector<int> adjs;
};

struct NodeComparator {
	NodeComparator(const vector<Node> & all_nodes) { this->_all_nodes = all_nodes; }
	bool operator () (const int & i, const int & j) {
		return _all_nodes[i].degree < _all_nodes[j].degree;
	}

	vector<Node> _all_nodes;
};

bool checkVectorFull(const vector<int> & _vec );

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

	int getSize () const{
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

	T& operator[](int idx) const{
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
	// note: the indexing of entries in the matrix starts from 1
	// not zero!
private:
	
	int row_size;
	int column_size;
	int num_nonzeros;

	// dense representation
	T ** entries;

	// coordination representation
	int * row_idx_coo;
	int * column_idx_coo;
	T * entries_coo;

public:

	Matrix(){
		num_nonzeros = 0;
		row_size = 0;
		column_size = 0;
		entries = NULL;
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

	void setSize(const size_t & row_size, const size_t & column_size){
		if(entries != NULL){
			throw("Error setting size of matrix: already initialized!");
		}
		else{
			this->row_size = row_size;
			this->column_size = column_size; 
			entries = new T*[row_size];
			for (int i = 0; i < row_size; i++){
				entries[i] = new T[column_size];
			}

			for (int i = 0; i < row_size; i++)
				for (int j = 0; j < column_size; j++)
					entries[i][j] = 0.0;
		}
	}

	void setEntry(const int & r, const int & c, const T & data){
		if(r < 0 || r >= row_size || c < 0 || c >= column_size){
			cout << "row_size: " << row_size << " column_size: " << column_size << endl;
			cout << "r: " << r << " c:" << c << endl;
			throw("Error adding entry to matrix: index out of bound!");
		}
		else
			entries[r][c] = data;

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

				entries[r - 1][c - 1] = 1.0;
				row_idx_coo[idx_coo] = r;
				column_idx_coo[idx_coo] = c;
				entries_coo[idx_coo] = 1.0;
				++idx_coo;
				//cout << idx_coo << endl;
			}
		}
	}
	//template<typename T>
	Vector<T> multiplyVectorPlain(const Vector<T> & vec){
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
					ret[i] +=  entries[i][j] * vec[j];
				}
			}
		}
		//cout << "Complete" << endl;
		return ret;
	}

	Vector<T> multiplyVectorCOO(const Vector<T> & vec){
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

	Matrix<T> getPermutedForm(const vector<int> & row_perm, const vector<int> & columns_perm){
		Matrix<T> ret;
		if(row_perm.size() != row_size || columns_perm.size() != column_size)
			throw("Cannot Permute: size does not match!");
		else{
			
			ret.setSize(row_size, column_size);
			for(int i = 0; i < row_size; i++){
				for(int j = 0; j < column_size; j++){
					int new_r = row_perm[i] - 1;
					int new_c = columns_perm[j] - 1;
					ret.setEntry(i, j, entries[new_r][new_c]);
				}
			}
		}
		return ret;
	}

	void writeToFile(const string & file){
		ofstream os;
		os.open(file);
		os << row_size << " " <<column_size << " " << num_nonzeros << endl;
		for(int i = 0; i < row_size; i++){
			for(int j = 0; j < column_size; j++){
				if(abs(entries[i][j])>0.0000001){
					os << i+1 << " " << j+1 << " " << entries[i][j] << endl;
				}
			}
		}
		os.close();
	}


	int findUnvisitedLowestDegreeNode(const vector<Node> & all_nodes,  bool* & visited) const{
		int min_degree = INT_MAX;
		int min_degree_node_id = -1;
		for (int i = 0; i < row_size + column_size; i++){
			if (visited[i] == false && all_nodes[i].degree < min_degree){
				min_degree = all_nodes[i].degree;
				min_degree_node_id = i;
			}
			if (min_degree == 0)
				break;
		}
		return min_degree_node_id;
	}

	void findRCM(vector<int> & row_perm, vector<int> & column_perm){
		vector<int> ret;
		queue<int> Q;

		bool* visited = new bool[row_size + column_size];
		for (int i = 0; i < row_size + column_size; i++){
			visited[i] = false;
		}
		// indexing from 1 instead of 0
		vector<Node> all_nodes(row_size + column_size);		
		for (int k = 0; k < num_nonzeros; k++){
			int n1 = row_idx_coo[k] - 1;
			int n2 = column_idx_coo[k] + row_size - 1;
			all_nodes[n1].adjs.push_back(n2);
			all_nodes[n1].degree++;
			all_nodes[n2].adjs.push_back(n1); 
			all_nodes[n2].degree++;
		}
		NodeComparator nodecomparator(all_nodes);

		while(ret.size() < row_size + column_size){
			int newnode = findUnvisitedLowestDegreeNode(all_nodes, visited);
			if(newnode == -1){
				cout << "newnode = -1 is wrong" <<endl;
				exit(1);
			}
			Q.push(newnode);
			cout << "Foudn newnode " << newnode << endl;
			//visited[newnode] = true;

			while( Q.size() > 0){
				//cout << "Q.size() "<<  Q.size() << endl;
				int p = Q.front();
				Q.pop();
				ret.push_back(p);
				//cout << "Pushed in ret " << p << " Q.size() "<<  Q.size() << endl;
				visited[p] = true;
				
				vector<int> adjs;
				adjs.clear();
				for (int i = 0; i < all_nodes[p].degree; i++){
					int adj = all_nodes[p].adjs[i];
					if (visited[adj] == false){
						adjs.push_back(adj);
					}
				}
				std::sort(adjs.begin(), adjs.end(), nodecomparator);
				for(int i = 0; i < adjs.size(); i++){
					Q.push(adjs[i]);
					//cout << "Pusing to Q " << adjs[i] << endl;
				}

			}
		}

		row_perm.clear();
		column_perm.clear();
		for(int i = 0; i < row_size + column_size; i++){
			if(ret[i] > row_size - 1)
				column_perm.push_back(ret[i] - row_size + 1);
			else
				row_perm.push_back(ret[i] + 1);

		}
		/*
		cout << "Here shows the column permutation" << endl;
		for(int i = 0; i < column_perm.size(); i++){
			cout << column_perm[i] << endl;
		}
		
		if(checkVectorFull(column_perm) == false){
			throw("Returned column permutation not correct!");
		}

		cout << "Here shows the row permutation" << endl;
		for(int i = 0; i < row_perm.size(); i++){
			cout << row_perm[i] << endl;
		}
		if(checkVectorFull(row_perm) == false){
			throw("Returned row permutation not correct!");
		}
		*/

	}

	void findRCMOrdering(vector<int> & row_perm, vector<int> & column_perm){
		vector<int> ret(row_size + column_size);
		int ret_idx = 0;

		// indexing from 1 instead of 0
		vector<Node> all_nodes(row_size + column_size);
		bool* visited = new bool[row_size + column_size];
		for (int i = 0; i < row_size + column_size; i++){
			visited[i] = false;
		}

		for (int k = 0; k < num_nonzeros; k++){
			int n1 = row_idx_coo[k] - 1;
			int n2 = column_idx_coo[k] + row_size - 1;
			all_nodes[n1].adjs.push_back(n2);
			all_nodes[n1].degree++;
			all_nodes[n2].adjs.push_back(n1); 
			all_nodes[n2].degree++;
		}

		int min_degree = INT_MAX;
		int min_degree_node_id = 0;
		for (int i = 0; i < row_size + column_size; i++){
			if (all_nodes[i].degree < min_degree){
				min_degree = all_nodes[i].degree;
				min_degree_node_id = i;
			}
			if (min_degree == 0)
				break;
		}
		ret[ret_idx++] = min_degree_node_id;
		visited[min_degree_node_id] = true;


		vector<int> adj_nodes;
		NodeComparator nodecomparator(all_nodes);

		while (ret_idx != row_size + column_size){

		
			adj_nodes.clear();
			int curr_node_id = ret[ret_idx-1];
			bool pushed = false;
			for (int i = 0; i < all_nodes[curr_node_id].degree; i++){
				int temp = all_nodes[curr_node_id].adjs[i];
				if (visited[temp] == false){
					adj_nodes.push_back(temp);
					pushed = true;
				}
			}
			if (pushed == true){
				//std::sort(adj_nodes.begin(), adj_nodes.end());
				std::sort(adj_nodes.begin(), adj_nodes.end(), nodecomparator);
				for (size_t i = 0; i < adj_nodes.size(); i++){
					ret[ret_idx++] = adj_nodes[i];
					visited[adj_nodes[i]] = true;
				}
			}
			else{
				min_degree = INT_MAX;
				for (int i = 0; i < row_size + column_size ; i++){
					if (visited[i] == false && all_nodes[i].degree < min_degree ){
						min_degree = all_nodes[i].degree;
						min_degree_node_id = i;
					}
					if (min_degree == 0)
						break;
				}
				ret[ret_idx++] = min_degree_node_id;
				visited[min_degree_node_id] = true;
			}
		}

		row_perm.clear();
		column_perm.clear();
		for(int i = 0; i < row_size + column_size; i++){
			if(ret[i] > row_size - 1)
				column_perm.push_back(ret[i] - row_size + 1);
			else
				row_perm.push_back(ret[i] + 1);

		}

		if(checkVectorFull(column_perm) == false){
			throw("Returned column permutation not correct!");
		}
		if(checkVectorFull(row_perm) == false){
			throw("Returned row permutation not correct!");
		}

		//return std::move(ret);
	}
};

bool checkVectorFull(const vector<int> & _vec ){
	vector<int> vec = _vec;
	std::sort(vec.begin(), vec.end());
	for(int i = 0 ; i < vec.size(); i++){
		if(vec[i] != i+1){
			cout << i+1 << " " << vec[i] << endl;
			return false;
		}
	}
	return true;
}

int main(int argc, char * argv[]){


	Matrix<double> mtx;
	string file = "./matrices/rand4.mtx";

	for(int i = 0; i < argc; i++){
		cout << "Argument " << i << " is " << argv[i] << endl;
	}
	
	if(argc == 2){
		file = argv[1];
	}
	cout << file << endl;
	int a;
	cin >> a;
	mtx.readFromFile(file);
	//mtx.showInfo();

	Vector<double> vec;
	vec.fillWithRandom(mtx.getColumnSize());
	//vec.displayContent();

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

	try{
		t5 = clock();
		vector<int> row_ordering, column_ordering;


		mtx.findRCM(row_ordering, column_ordering);
		
		// for(int i ; i < row_ordering.size(); i++){
		// 	cout << row_ordering[i] << endl;
		// }
		// for(int i ; i < column_ordering.size(); i++){
		// 	cout << column_ordering[i] << endl;
		// }

		Matrix<double> mtx_perm = mtx.getPermutedForm(row_ordering, column_ordering);
		t6 = clock();
		mtx_perm.writeToFile(file + ".perm");
	}
	catch (char const * ex){
		cout << ex << endl;
	}

	//int a;
	//cin >>  a;
	//result2.displayContent();

	printf("%10.9f seconds elapsed.\n", (t2 - t1) / (double)CLOCKS_PER_SEC);
	printf("%10.9f seconds elapsed.\n", (t4 - t3) / (double)CLOCKS_PER_SEC);
	printf("%10.9f seconds elapsed.\n", (t6 - t5) / (double)CLOCKS_PER_SEC);
	printf("CLOCKS_PER_SEC = %10.9f.\n", (double)CLOCKS_PER_SEC);
	return 0;
}