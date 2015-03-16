/*
   Author : Joydeep
   Date   : 16 nov 2013
   App    : Dail-a-ride  
*/
#include<stdio.h>
#include<stdlib.h>

#define MAX_VAL 99999

int nodes, dist[100][100], totalCabs, maxCap, totalReq;

int  inc = 0 ;


struct request{
	int reqId;
	int source;
	int destination;
	int startTime;
	int endTime;
	int flag;
	int cost;
	int pickupTime;
	int serviceCab;
	int dropTime;
};
typedef struct request req;
struct cabs{
	int cabId;
	int location;
	int revenue;
	int passengers;
	int time;
	req *requests[100];
							
};
typedef struct cabs cab;




void CalculateShortestPath(int dist[100][100], int nodes)
{
	int i,j,k;
	//Applying floyd warshalls algo
	
	for(k=0;k<nodes;k++)
	{
		for(i=0;i<nodes;i++)
		{
			for(j=0;j<nodes;j++)
			{
				if(dist[i][k]+dist[k][j]< dist[i][j])
				{
					dist[i][j]= dist[i][k]+dist[k][j];
				}
			}
		}
	}
	for(i=0; i<nodes; i++){
		for(j=0;j<nodes ;j++){
			printf("%d ",dist[i][j]);}
			printf("\n");}
}




void SortList(req *requests[10000])
{
	req *temp1;
	int i, j;
	
	
	for(j=0; j < totalReq; j++)
	{
		for (i=0 ;i < totalReq-1 ; i++)
		{
			if(requests[i]->endTime > requests[i+1]->endTime)
			{
				temp1= requests[i];
				requests[i] = requests[i+1];
				requests[i+1]= temp1;
				
			}
		}
	}
	

	
}





void DropPassengers(cab *cabss)
{
	
	int i, cost=999999, index=100;
	req *temp;

							
	// drop the passenger that gives min revenue
	for(i=0; i< (cabss->passengers) ; i++)
	{
		if(cost > dist[(cabss->location)-1][((cabss->requests[i])->destination) -1])         
		{
			cost = dist[(cabss->location)-1][((cabss->requests[i])->destination) -1] ;
			index = i;
		}
	}
	
	cabss->location = (cabss->requests[index])->destination ;
	//cabss->revenue += cost;
	cabss->time += 2*cost;
	(cabss->requests[index])->dropTime = cabss->time;
	//(cabss->requests[index])->cost +=cost;
	cabss->revenue += dist[cabss->requests[index]->source][cabss->requests[index]->destination];                    //latest modification
	i=cabss->passengers -1;	
	if(i != index)
	{
		temp = cabss->requests[i];
	}	
	else
	{
		i--;
		temp = cabss->requests[i];
	}
		
	cabss->requests[index]=temp;
	cabss->passengers--;
}






//finding a suitable cab
void FindCab(cab *cabss[1000], req *requests)
{
	int reqPosition, cabPosition, i, index = 0, distance = 99999, temp=0;

	reqPosition = requests->source;
	for(i=0; i < totalCabs; i++)                                    
	{
		if((cabss[i]->passengers)-1 < maxCap )
		{
			temp = dist[(cabss[i]->location)-1][requests->source-1]; 			
			if(temp<distance )				
			{	                 						
					distance = temp;           
					index=i;	
			}	
		}
	}
						
	//can that cab reach there before finishing time
	if((distance*2 + cabss[index]->time  <= requests->endTime)&& totalCabs > 0 && maxCap >0)
	{
		
		//request that cab to go there and update details
		//cabss[index]->revenue += distance;
		if(distance*2 + cabss[index]->time < requests->startTime)
		{
			cabss[index]->time =requests->startTime;
		}
		else
		{
			cabss[index]->time =distance*2+cabss[index]->time;
		}
		cabss[index]->location= requests->source;

		for(i=0 ; i< cabss[index]->passengers; i++)
		{
			//(cabss[index]->requests[i])->cost +=distance;
		}
		
		cabss[index]->requests[cabss[index]->passengers]=requests;
		cabss[index]->passengers++;
		requests->serviceCab= cabss[index]->cabId;
		requests->pickupTime = cabss[index]->time;
	}


	for(i=0; i < totalCabs; i++)
	{
		if(cabss[i]->passengers == maxCap && maxCap > 0 )
		{
			
			DropPassengers(cabss[i]);
		}
	}
	
	if(totalCabs > 0 && maxCap> 0)
	printf("\ncabs  %d \nposition %d revenue %d \ntime %d \npassengers %d",index,cabss[index]->location, cabss[index]->revenue,cabss[index]->time, cabss[index]->passengers);			
}








// select req will be called for the first time
void SelectReq(req *requests[10000], cab *cabss[1000])
{
	int i, reqId=0, j=0, smallestTime=10000;
	
		//selecting req which has not been dropped
		
		FindCab(cabss , requests[inc]);
		
		requests[inc]->flag =1;
		inc++;	
	
	
}









int main(int argc, char *argv[]){
	
	cab *cabsObj[1000];
	req *reqObj[10000];
	int **ptr;
	
	int i, j, check, profit =0;

	if( argc == 2 )
	{
  	        printf("The argument supplied is %s\n", argv[1]);
	}
	else 
	{
		printf("Too many arguments supplied.\n");
		exit(3);
	}
	
	FILE *fp;

	fp= fopen(argv[1], "r");

	check= fscanf(fp,"%d %d %d %d", &nodes, &totalCabs, &maxCap, &totalReq );

	if(check != 4)
	{
		printf("Error!!");
		exit(1);
	}

	printf("%d  %d  %d  %d  ", nodes, totalCabs, maxCap, totalReq);
	printf("\n");
	check = 0;

	//taking imput between distances
	for(i = 0 ; i< nodes ; i++)
	{
		for(j = 0; j < nodes; j++)
		{
			check += fscanf(fp, "%d ", &dist[i][j]);
			if (dist[i][j]== -1 && i!=j)
			{
				dist[i][j]= MAX_VAL;	
			}
			if(i==j)
			{
				dist[i][j]=0;
			}
		}
	}

	if(check != nodes*nodes)
	{
		printf("Error!!");
		exit(1);
	}

	check = 0;

	CalculateShortestPath(dist, nodes);
	for(i = 0 ; i < nodes ; i++)
	{
		for(j = 0; j< nodes; j++)
		{
			//printf("%d ", dist[i][j]);
		}
		//printf("\n");
	}

	

	for(i=0; i < totalCabs; i++)
	{	
		cabsObj[i]=  (cab *)malloc (sizeof(cab)* totalCabs);
		cabsObj[i]->cabId = i;
		check = fscanf(fp, "%d ",&(cabsObj[i]->location ));
		for(j=0; j< maxCap; j++)
		{
			cabsObj[i]->requests[j] = (req *)malloc(sizeof(req));
	
		}
		cabsObj[i]->time=0;
				
		if(check != 1)
		{
			printf("error!");
			exit(0);
		}
		else
		{
			check=0;
		} 
		
	}

	check =0;
	for(i = 0 ; i < totalReq ; i++)
	{
		reqObj[i]=  (req *)malloc (sizeof(req)* totalReq);
		check = fscanf(fp, "%d ",&(reqObj[i]->source ));
		check += fscanf(fp, "%d ",&(reqObj[i]->destination ));
		check += fscanf(fp, "%d ",&(reqObj[i]->startTime ));
		check += fscanf(fp, "%d ",&(reqObj[i]->endTime ));
		if(check != 4)
		{
			printf("error!");
			exit(0);
		}
		else
		{
			check=0;
		}
		reqObj[i]->flag=0;
		reqObj[i]->cost = 0 ;
		reqObj[i]->reqId = i+1;
	}


	//CALLING a function which will sort the record list
	SortList(reqObj); 


	

	// we want to have a look on all requests
	for(i = 0 ; i < totalReq/*totalReq*/; i++)
	{
		//calling schedule
		SelectReq(reqObj, cabsObj);
	
	}

	//dropping after midnight										
	for(i=0 ; i< totalCabs ; i++)
	{
		//printf("\n\n\nCab %d has total %d passenger ",i+1,cabsObj[i]->passengers );
		j=0; 
		while( cabsObj[i]->passengers >0 && maxCap>0)
		{
			printf("\n\n passenger no %d ", j++);
			DropPassengers(cabsObj[i]);
		}
	}


	//printing request
	for(i = 0 ; i < totalReq/*totalReq*/; i++)
	{
		printf("\n\nReq no %d Pickup Time %d drop time %d Service Cab %d",reqObj[i]->reqId,reqObj[i]->pickupTime,reqObj[i]->dropTime,reqObj[i]->serviceCab);

	}

	//revenue generated by each cab
	printf("\n\n\n");
	for(i = 0 ; i < totalCabs && maxCap > 0 ; i++)
	{
		printf("\n\n");
		profit += cabsObj[i]->revenue;
		printf("\n the profit of cab %d is %d",i+1, cabsObj[i]->revenue );
	}
	
	printf("\n\n\n*******************  the total revenue %d  ***************************\n\n" , profit);

	
	
	return 0;
}



