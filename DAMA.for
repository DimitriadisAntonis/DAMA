  program proto
	dimension a(16,16),e(16,16)
	integer a
	character e
	common /itoc/a,e
	
	
	a=5
	b=0
  do 2 l=6,12,2
	a(5,l)=1
	a(6,l-1)=1
	a(7,l)=1
	a(11,l)=0
	a(10,l-1)=0
	a(12,l-1)=0
2	enddo
  do 50 i=1,16
	do 50 l=1,4
	a(i,l)=4
	a(i,l+12)=4
50enddo
  do 60 i=1,16
	a(1,i)=4
	a(2,i)=4
	a(3,i)=4
	a(4,i)=4
	a(13,i)=4
	a(14,i)=4
	a(15,i)=4
	a(16,i)=4
60enddo
  write(*,*)
  call ito
	write(*,*)'     1.',' 2.',' 3.',' 4.',' 5.',' 6.',' 7.',' 8.'
  write(*,*)'   ***********DAMA***********'
	write(*,*)'   |','***   ***   ***   ***   ','|'  
	write(*,*)' 1.','|','*** ',e(5,6),' *** ',e(5,8),' *** ',e(5,10),' *** ',e(5,12),' |'
	write(*,*)'   |','***   ***   ***   ***   ','|'
	write(*,*)'   |','   ***   ***   ***   ***','|'  
	write(*,*)' 2.','| ',e(6,5),' *** ',e(6,7),' *** ',e(6,9),' *** ',e(6,11),' ***|'
	write(*,*)'   |','   ***   ***   ***   ***','|'
	
	write(*,*)'   |','***   ***   ***   ***   ','|'  
	write(*,*)' 3.','|','*** ',e(7,6),' *** ',e(7,8),' *** ',e(7,10),' *** ',e(7,12),' |'
	write(*,*)'   D','***   ***   ***   ***   ','D'

	write(*,*)'   A','   ***   ***   ***   ***','A'  
	write(*,*)' 4.','M ',e(8,5),' *** ',e(8,7),' *** ',e(8,9),' *** ',e(8,11),' ***M'
	write(*,*)'   A','   ***   ***   ***   ***','A'

	
	write(*,*)'   |','***   ***   ***   ***   ','|'  
	write(*,*)' 5.','|','*** ',e(9,6),' *** ',e(9,8),' *** ',e(9,10),' *** ',e(9,12),' |'
	write(*,*)'   |','***   ***   ***   ***   ','|'
	
	write(*,*)'   |','   ***   ***   ***   ***','|'  
	write(*,*)' 6.','| ',e(10,5),' *** ',e(10,7),' *** ',e(10,9),' *** ',e(10,11),' ***|'
	write(*,*)'   |','   ***   ***   ***   ***','|'
	
	write(*,*)'   |','***   ***   ***   ***   ','|'  
	write(*,*)' 7.','|','*** ',e(11,6),' *** ',e(11,8),' *** ',e(11,10),' *** ',e(11,12),' |'
	write(*,*)'   |','***   ***   ***   ***   ','|'

	
	write(*,*)'   |','   ***   ***   ***   ***','|'  
	write(*,*)' 8.','| ',e(12,5),' *** ',e(12,7),' *** ',e(12,9),' *** ',e(12,11),' ***|'
	write(*,*)'   |','   ***   ***   ***   ***','|'
	write(*,*)'   **********DAMA************' 
    
      
170read(*,*) f,g
	f=f+4
	g=g+4
	read(*,*) h,j
	h=h+4
	j=j+4
	if(a(f,g)==3)then
	a(f,g)=5
	a(h,j)=3
	goto 81
	else
	a(f,g)=5
	a(h,j)=0
	endif
  if(f==5)a(h,j)=3
	if(h==5)a(h,j)=3
81if((f-h)<0.and.(g-j)<0)then
	v=f
	m=g
	n=h
	r=j
	f=h
	h=v
	g=j
	j=m
	endif
	if((f-h)<0.and.(g-j)>0)then
	v=f
	m=g
	n=h
	r=j
	f=h
	h=v
	g=j
	j=m
	endif
  if((f-h)==2.and.(g-j)==2)then
  a(f-1,g-1)=5
	endif
  if((f-h)==4.and.(g-j)==4)then
	a(f-1,g-1)=5
	a(f-3,g-3)=5
	endif
	if((f-6)>0.and.(g-6)>0)then
  if((f-h)==6.and.(g-j)==6.and.a(f-6,g-6)==5)then
	a(f-1,g-1)=5
	a(f-3,g-3)=5
	a(f-5,g-5)=5
	endif	
	endif
 	if((f-5)>0.and.(g-4)>0)then
 	if((f-h)==6.and.(g-j)==2.and.a(f-4,g-4)==5)then
	a(f-1,g-1)=5
	a(f-3,g-3)=5
	a(f-5,g-3)=5
	endif
	endif
	if((f-3)>0.and.(g-2)>0)then
  if((f-h)==4.and.(g==j).and.a(f-2,g-2)==5)then
	a(f-1,g-1)=5
	a(f-3,g-1)=5
	endif
	endif
	if((f-6)>0.and.(g-2)>0)then
  if((f-h)==6.and.(g-j)==2.and.a(f-6,g-2)==5)then
	a(f-1,g-1)=5
	a(f-3,g-1)=5
	a(f-5,g-1)=5
	endif
	endif
	if((f-5)>0)then
  if((f-h)==6.and.(j-g)==2.and.a(f-2,g-2)==5)then
	a(f-1,g-1)=5
	a(f-3,g-1)=5
	a(f-5,g+1)=5
	endif
	endif
  if((f-h)==2.and.(j-g)==2)then
	a(f-1,g+1)=5
	endif
  if((f-3)>0)then
	if((f-h)==4.and.(j-g)==4)then
	a(f-1,g+1)=5
	a(f-3,g+3)=5
	endif
	endif
  if((f-3)>0)then
	if((f-h)==4.and.j==g.and.a(f-2,g+2)==5)then
	a(f-1,g+1)=5
	a(f-3,g+1)=5
	endif
	endif
	if((f-5)>0)then
	if((f-h)==6.and.(j-g)==2.and.a(f-2,g+2)==5)then
	a(f-1,g+1)=5
	a(f-3,g+1)=5
	a(f-5,g+1)=5
	endif
	endif
	if((f-5)>0)then
	if((f-h)==6.and.(j-g)==2.and.a(f-4,g+4)==5)then
	a(f-1,g+1)=5
	a(f-3,g+3)=5
	a(f-5,g+3)=5
	endif
	endif
  b=0
  do 42 c=5,12
	do 42 d=5,12
	i=c
	l=d
	
	if(a(i,l)==2)then           
56if(i<=10)then
  if(a(i+3,l+3)==0)goto 57
  if(a(i,l)==2.and.(a(i+1,l+1)==0.or.a(i+1,l+1)==3).and.a(i+2,l+2)==5)then
	a(i,l)=5
	a(i+1,l+1)=5
	a(i+2,l+2)=2
	i=i+2
	l=l+2
	b=1
	goto 56
  endif
	endif
57if(i<=10)then
  if(a(i+3,l-3)==0)goto 58
  if(a(i,l)==2.and.(a(i+1,l-1)==0.or.a(i+1,l-1)==3).and.a(i+2,l-2)==5)then
	a(i,l)=5
	a(i+1,l-1)=5
	a(i+2,l-2)=2
	i=i+2
	l=l-2
	b=1
	goto 56
  endif
	endif
58if(i>=7)then
  if(a(i-3,l+3)==0)goto 59
  if(a(i,l)==2.and.(a(i-1,l+1)==0.or.a(i-1,l+1)==3).and.a(i-2,l+2)==5)then
	a(i,l)=5
	a(i-1,l+1)=5
	a(i-2,l+2)=2
	i=i-2
	l=l+2
	b=1
	goto 56
  endif 
	endif
59if(i>=7)then
  if(a(i-3,l-3)==0)goto 160
  if(a(i,l)==2.and.(a(i-1,l-1)==0.or.a(i-1,l-1)==3).and.a(i-2,l-2)==5)then
	a(i,l)=5
	a(i-1,l-1)=5
	a(i-2,l-2)=2
	i=i-2
	l=l-2
	b=1
	goto 56
  endif
	endif
	
160if(b==1)goto 46
     
156if(a(i,l)==2.and.a(i+1,l+1)==5.and.(a(i+2,l+2)/=0.and.a(i+2,l+2)/=3))then
  if((a(i+2,l)/=0.and.a(i+2,l)/=3).or.a(i,l+2)/=5)then
	if(a(i,l+2)/=3.or.a(i+2,l)/=5)then
	if((a(i+1,l-1)/=1.and.a(i+1,l-1)/=2).or.(a(i+2,l-2)/=0.and.a(i+2,l-2)/=3))then
	a(i,l)=5
	a(i+1,l+1)=2
	i=i+1
	l=l+1
	b=1
	goto 46
	endif
	endif
	endif
	endif
  if(a(i,l)==2.and.a(i+1,l-1)==5.and.(a(i+2,l-2)/=0.and.a(i+2,l-2)/=3))then
  if((a(i+2,l)/=0.and.a(i+2,l)/=3).or.a(i,l-2)/=5)then
	if(a(i+2,l)/=5.or.a(i,l-2)/=3)then
  if((a(i+1,l+1)/=1.and.a(i+1,l+1)/=2).or.(a(i+2,l+2)/=0.and.a(i+2,l+2)/=3))then
	a(i,l)=5
	a(i+1,l-1)=2
	i=i+1
	l=l-1
	b=1
	goto 46
	endif
	endif
	endif
	endif
  if(a(i,l)==2.and.a(i-1,l-1)==5.and.a(i-2,l-2)/=3)then
	if(a(i-2,l)/=3.or.a(i,l-2)/=5)then
  if((a(i,l-2)/=0.and.a(i,l-2)/=3).or.a(i-2,l)/=5)then
  if((a(i+1,l-1)/=1.and.a(i+1,l-1)/=2).or.(a(i+2,l-2)/=0.and.a(i+2,l-2)/=3))then
  if((a(i+1,l+1)/=1.and.a(i+1,l+1)/=2).or.(a(i+2,l+2)/=0.and.a(i+2,l+2)/=3))then
	a(i,l)=5
	a(i-1,l-1)=2
	i=i-1
	l=l-1
	b=1
	goto 46
	endif
	endif
	endif
	endif
	endif
  if(a(i,l)==2.and.a(i-1,l+1)==5.and.a(i-2,l+2)/=3)then
	if(a(i-2,l)/=3.or.a(i,l-2)/=5)then
  if((a(i,l-2)/=0.and.a(i,l-2)/=3).or.a(i-2,l)/=5)then
	if((a(i+1,l-1)/=1.and.a(i+1,l-1)/=2).or.(a(i+2,l-2)/=0.and.a(i+2,l-2)/=3))then
  if((a(i+1,l+1)/=1.and.a(i+1,l+1)/=2).or.(a(i+2,l+2)/=0.and.a(i+2,l+2)/=3))then
	a(i,l)=5
	a(i-1,l+1)=2
	i=i-1
	l=l+1
	b=1
	goto 46
	endif
	endif
	endif
	endif
	endif
	if(b==1)goto 46
	endif
	
41if((a(i,l)==1).and.(a(i+1,l+1)==0.or.a(i+1,l+1)==3).and.a(i+2,l+2)==5)then
	a(i,l)=5
	a(i+1,l+1)=5
	a(i+2,l+2)=1
	i=i+2
	l=l+2
	b=1
	goto 41
	endif
  if(a(i,l)==1.and.(a(i+1,l-1)==0.or.a(i+1,l-1)==3).and.a(i+2,l-2)==5)then
	a(i,l)=5
	a(i+1,l-1)=5
	a(i+2,l-2)=1
	i=i+2
	l=l-2
	b=1
	goto 41
	endif
  if(a(i,l)==1.and.i==12)a(i,l)=2	
	if(b==1)goto 46
	
42enddo
	if(b==1)goto 46
	 
  do 112 c=12,5,-1
	do 112 d=5,12
	i=c
	l=d
43if(a(i,l)==1.and.a(i+1,l+1)==5)then
  if(a(i+2,l+2)/=0.and.a(i+2,l+2)/=3)then
	if((a(i+2,l)/=0.and.a(i+2,l)/=3).or.a(i,l+2)/=5)then
	if((a(i+1,l-1)/=1.and.a(i+1,l-1)/=2).or.(a(i+2,l-2)/=0.and.a(i+2,l-2)/=3))then
	a(i,l)=5
	a(i+1,l+1)=1
	i=i+1
	l=l+1
  if(a(i,l)==1.and.i==12)a(i,l)=2	
	b=1
	endif
	endif
	endif
	endif
  if(b==1)goto 46
44if(a(i,l)==1.and.a(i+1,l-1)==5)then
  if(a(i+2,l-2)/=0.and.a(i+2,l-2)/=3)then
	if((a(i+2,l)/=0.and.a(i+2,l)/=3).or.a(i,l-2)/=5)then
  if((a(i+1,l+1)/=1.and.a(i+1,l+1)/=2).or.(a(i+2,l+2)/=0.and.a(i+2,l+2)/=3))then
	a(i,l)=5
	a(i+1,l-1)=1
	i=i+1
	l=l-1
  if(a(i,l)==1.and.i==12)a(i,l)=2	
	b=1
	endif
	endif
	endif
	endif
  if(b==1)goto 46
112enddo
      
  do 113 c=12,5,-1
	do 113 d=5,12
	i=c
	l=d
  if(a(i,l)==1.and.a(i+1,l+1)==5)then
	a(i,l)=5
	a(i+1,l+1)=1
	i=i+1
	l=l+1
  if(a(i,l)==1.and.i==12)a(i,l)=2	
	b=1
	endif
	if(b==1)goto 46
  if(a(i,l)==1.and.a(i+1,l-1)==5)then
	a(i,l)=5
	a(i+1,l-1)=1
	i=i+1
	l=l-1
  if(a(i,l)==1.and.i==12)a(i,l)=2	
	b=1
	endif
	if(b==1)goto 46
  if(a(i,l)==2.and.a(i+1,l+1)==5)then
	a(i,l)=5
	a(i+1,l+1)=2
	i=i+1
	l=l+1
  if(a(i,l)==1.and.i==12)a(i,l)=2	
	b=1
	endif
  if(b==1)goto 46
	if(a(i,l)==2.and.a(i+1,l-1)==5)then
	a(i,l)=5
	a(i+1,l-1)=2
	i=i+1
	l=l-1
  if(a(i,l)==1.and.i==12)a(i,l)=2	
	b=1
	endif
	if(b==1)goto 46
	if(a(i,l)==2.and.a(i-1,l+1)==5)then
	a(i,l)=5
	a(i-1,l+1)=2
	i=i-1
	l=l+1
  if(a(i,l)==1.and.i==12)a(i,l)=2	
	b=1
	endif
	if(b==1)goto 46
	if(a(i,l)==2.and.a(i-1,l-1)==5)then
	a(i,l)=5
	a(i-1,l-1)=2
	i=i-1
	l=l-1
  if(a(i,l)==1.and.i==12)a(i,l)=2	
	b=1
	endif
  if(b==1)goto 46
113enddo
      
46if(a(i,l)==1.and.i==12)a(i,l)=2	
  call ito
	write(*,*)'     1.',' 2.',' 3.',' 4.',' 5.',' 6.',' 7.',' 8.'
  write(*,*)'   ***********DAMA***********'

	write(*,*)'   |','***   ***   ***   ***   ','|'  
	write(*,*)' 1.','|','*** ',e(5,6),' *** ',e(5,8),' *** ',e(5,10),' *** ',e(5,12),' |'
	write(*,*)'   |','***   ***   ***   ***   ','|'
	
	write(*,*)'   |','   ***   ***   ***   ***','|'  
	write(*,*)' 2.','| ',e(6,5),' *** ',e(6,7),' *** ',e(6,9),' *** ',e(6,11),' ***|'
	write(*,*)'   |','   ***   ***   ***   ***','|'
	
	write(*,*)'   |','***   ***   ***   ***   ','|'  
	write(*,*)' 3.','|','*** ',e(7,6),' *** ',e(7,8),' *** ',e(7,10),' *** ',e(7,12),' |'
	write(*,*)'   D','***   ***   ***   ***   ','D'

	write(*,*)'   A','   ***   ***   ***   ***','A'  
	write(*,*)' 4.','M ',e(8,5),' *** ',e(8,7),' *** ',e(8,9),' *** ',e(8,11),' ***M'
	write(*,*)'   A','   ***   ***   ***   ***','A'

	
	write(*,*)'   |','***   ***   ***   ***   ','|'  
	write(*,*)' 5.','|','*** ',e(9,6),' *** ',e(9,8),' *** ',e(9,10),' *** ',e(9,12),' |'
	write(*,*)'   |','***   ***   ***   ***   ','|'
	
	write(*,*)'   |','   ***   ***   ***   ***','|'  
	write(*,*)' 6.','| ',e(10,5),' *** ',e(10,7),' *** ',e(10,9),' *** ',e(10,11),' ***|'
	write(*,*)'   |','   ***   ***   ***   ***','|'
	
	write(*,*)'   |','***   ***   ***   ***   ','|'  
	write(*,*)' 7.','|','*** ',e(11,6),' *** ',e(11,8),' *** ',e(11,10),' *** ',e(11,12),' |'
	write(*,*)'   |','***   ***   ***   ***   ','|'

	
	write(*,*)'   |','   ***   ***   ***   ***','|'  
	write(*,*)' 8.','| ',e(12,5),' *** ',e(12,7),' *** ',e(12,9),' *** ',e(12,11),' ***|'
	write(*,*)'   |','   ***   ***   ***   ***','|'
	write(*,*)'   **********DAMA************'

  do 158 i=5,12
	do 158 l=5,12
  if(a(i,l)==1.or.a(i,l)==2)then
	goto 159
	endif
158enddo
  write(*,*)'I lost!'
	goto 172
159do 53 i=5,12
	do 53 l=5,12
  if(a(i,l)==0.or.a(i,l)==3)then
	goto 170
	endif
53enddo
171write(*,*)'You lost!'
  read(*,*)i
172end
	
	subroutine ito()
	implicit none
	common /itoc/a,e
	dimension a(16,16),e(16,16)
	integer a,i,l
	character e
	do 13 i=5,12
	do 13 l=5,12
	if(a(i,l)==5)e(i,l)=''
	if(a(i,l)==1)e(i,l)='#'
	if(a(i,l)==0)e(i,l)='$'
	if(a(i,l)==2)e(i,l)='K'
	if(a(i,l)==3)e(i,l)='L'
13enddo
    
	end
