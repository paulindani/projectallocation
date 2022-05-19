#Suppose that there are nb.of.students students and nb.groups groups which correspond to different dissertation projects
#The number of students in each group is fixed at according to the group.sizes vector
#The average marks of the students from Semester 1 are loaded in the average.mark vector
#The students were asked to rank the projects from 1 to nb.groups (1 is most preferred)

#This scripts finds an optimal allocation of students to projects such that the sum of average mark
#times the student's rank of project to which they are allocated is minimized,
#while the number of students in each project is maintained according to group.sizes

#The script is using the Jonker-Volgenant algorithm solving the Assignment problem.
#This script is accompanying the paper "Consultancy Style Dissertations in Statistics and Data
#Science: Why and How" by Serveh Sharifi Far et al.
#The paper contains a detailed desciption of this algorithm in the Appendix.

#Author: Daniel Paulin, University of Edinburgh

#Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
  
  #The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



library('TreeDist')

#Creating cost matrix
create.cost.matrix <- function(student.choices,average.mark,group.sizes)
{
    nb.of.students=nrow(student.choices)
    nb.groups=length(group.sizes)
    cost.matrix=matrix(nrow=nb.of.students,ncol=nb.of.students)
    group.indices=create.group.indices(group.sizes)
    
    for(it in 1:nb.of.students)
    {
      for(g in 1:nb.groups)
      {
        cost.matrix[it, group.indices[g,1]:group.indices[g,2]]=student.choices[it,g]*average.mark[it]
      }
    }
    return(cost.matrix)
}

#Auxiliary function
create.group.indices<- function(group.sizes)
{
  nb.groups=length(group.sizes)
  group.indices=matrix(nrow=nb.groups,ncol=2)
  group.indices[1,1]=1
  cs.group.sizes=cumsum(group.sizes)
  group.indices[2:nb.groups,1]=cs.group.sizes[1:(nb.groups-1)]+1
  group.indices[1:nb.groups,2]=cs.group.sizes
  return(group.indices)
}
create.inverse.indices<- function(group.sizes)
{
  nb.of.students=sum(group.sizes)
  nb.groups=length(group.sizes)
  group.indices=create.group.indices(group.sizes)
  inv.group.indices=rep(0,nb.of.students)
  for(g in 1:nb.groups)
  {
    inv.group.indices[group.indices[g,1]:group.indices[g,2]]=g
  }
  return(inv.group.indices)
}

student.info=read.csv("project_choices_deduplicated.csv")
nb.of.students=107
student.info=student.info[1:nb.of.students,]
average.mark=student.info$Average.mark
#Loading the average mark

#First project
student.choices=read.csv("project1_choices.csv")
#student.choices is a dataframe with nb.groups columns and nb.of.student rows
#column j in row i represents the order of preference that student i would give to project j
#So if project j is student i's first choice, then this is 1, if it's their last choice, then it is nb.groups
#This data was obtained from an online form that the students were asked to fill out

nb.groups=4
group.sizes=c(27,27,27,26)


cost.matrix=create.cost.matrix(student.choices,average.mark,group.sizes)
#Finding the best match by the Jonker-Volgenant algorithm
best.match=LAPJV(cost.matrix)
inv.group.indices=create.inverse.indices(group.sizes)
#Creating the assignment according to groups
assignment.project1=inv.group.indices[best.match$matching]



#Second project
student.choices=read.csv("project2_choices.csv")
group.sizes=c(29,22,28,28)

cost.matrix=create.cost.matrix(student.choices,student.info$Average.mark,group.sizes)
best.match=LAPJV(cost.matrix)
inv.group.indices=create.inverse.indices(group.sizes)
assignment.project2=inv.group.indices[best.match$matching]



save(assignment.project1,assignment.project2,file="allocation.Rdata")