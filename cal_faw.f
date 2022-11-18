c *** Making Profiles of 5th Stokes wave and 3rd Cnoidal wave **********
c                                2022/10/08 Produced by Katsuya Hirayama
c **********************************************************************
      parameter( kmax=100 )
      character atheory*15

      write(*,*)
      write(*,*) 'Input water depth [in meters]:'
      read(*,*) D
      write(*,*)
      write(*,*) 'Input wave period [in seconds]:'
      read(*,*) T
      write(*,*)
      write(*,*) 'Input wave height [in meters]:'
      read(*,*) H

      delD = D/real(kmax)
      delT = T/real(kmax)

 1000 write(*,*) 'Please select a wave theory:'
      write(*,*) '   [1: Stokes wave ; 2: Cnoidal wave]'
      read(*,*) itheory

      if(itheory.eq.1) atheory='Stokes wave'
      if(itheory.eq.2) atheory='Cnoidal wave'

      open(10,file='input.dat',status='unknown')
      write(10,'(a20,a15)') 'Properties of the ', atheory

      open(20,file='profile.dat',status='unknown')
      write(20,'(a30,a15)') 'Water surface elevation of ',atheory
      write(20,'(2a10)') '     Theta','       Eta'

      write(*,*) 'Input the terget phase which is normarized by 2*pi'
      write(*,*) '(per 0.01 from 0.00 to 1.00)'
      read(*,*) theta

      open(30,file='field.dat',status='unknown')
      write(30,'(a30,a15,a5,f10.5)') 'Velocity & Pressure of ',atheory,
     &                               ' at',theta
      write(30,'(4a10)') '       Z/D','         U',
     &                   '         W','         P'

      if( itheory.eq.1 )then

        call STK(5,D,T,H,RL,C)

        do kt = 0, kmax

          X1 = real(kt)*delT/T
          call STKETA(X1,ZS)
          write(20,'(2f10.5)') X1,ZS

          do kz = 0, kmax
            Z1 = real(kz)*delD/D - 1.
            call STKUWP(X1,Z1,U1,W1,P1)
            if( theta.eq.X1 )
     &        write(30,'(4f10.5)') Z1,U1,W1,P1
          end do

        end do

      else if( itheory.eq.2 )then

        call CND(3,D,T,H,RL,C)

        do kt = 0, kmax

          X1 = real(kt)*delT/T
          call CNDETA(X1,ZS)
          write(20,'(2f10.5)') X1,ZS

          do kz = 0, kmax
            Z1 = real(kz)*delD/D - 1.
            call CNDUWP(X1,Z1,U1,W1,P1)
            if(theta.eq.X1)
     &        write(30,'(4f10.5)') Z1,U1,W1,P1
          end do
 
        end do

      else

        write(*,'(i5,a)') itheory, ' is inputted now.'
        write(*,*) 'Please input a correct number'
        go to 1000

      end if

      close(20)
      close(30)

      write(10,'(a30,f10.4)') 'Water Depth [m] is',D
      write(10,'(a30,f10.4)') 'Wave Height [m] is',H
      write(10,'(a30,f10.4)') 'Wave Period [s] is',T
      write(10,'(a30,f10.4)') 'Wavelength  [m] is',RL
      write(10,'(a30,f10.4)') 'Wave Celerity [m/s] is',C
      close(10)

      write(*,*) 'normal terminated'
      pause
      end

      include "stk.for"
      include "cnd.for"
