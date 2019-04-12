      subroutine LOMBERS
     $(H,ZXH,N,N1MET,IVLSW,DUMP)
C
C     Rudolf Loeser, 1997 Jul 29
C---- Sets up VELSW for diffusion calculation.
C     (This is version 3 of LOMBERS.)
C     !DASH
      save
C     !DASH
      real*8 H, ZERO, ZXH
      integer I, IM, IP, IVLSW, LUEO, N, N1MET
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, VECOUT, HI, BYE
C
C               H(N), ZXH(N)
      dimension H(*), ZXH(*)
C
      call HI ('LOMBERS')
C     !BEG
      IVLSW = 0
C
      if(N1MET.eq.1) then
        IP = 0
        IM = 0
        do 100 I = 1,N
          if(H(I).ne.ZERO) then
            if(H(I).gt.ZERO) then
              IP = IP+1
            else
              IM = IM+1
            end if
          end if
  100   continue
C
        if((IP.gt.0).and.(IM.le.0)) then
          IVLSW = +1
        else if((IM.gt.0).and.(IP.le.0)) then
          IVLSW = -1
        end if
C     !EJECT
        if(DUMP) then
          call LINER  (3, LUEO)
          write (LUEO,101)
  101     format(' ','Calculation of method selection parameter IVLSW'
     $               ', based on the values of h and N1MET'//
     $           ' ','When N1MET = 1, then IVLSW is set = +1 if some ',
     $               'h are greater than 0, and none less than 0;'/
     $           ' ',36X,'-1 if some h are less than 0, and none ',
     $               'greater than 0;'/
     $           ' ',36X,' 0 otherwise.'/
     $           ' ','When N1MET not =1, then IVLSW is set = 0 ',
     $               'regardless of h.')
C
          call VECOUT (LUEO, H,   N, 'h = g - f*ZXH :'                )
          call VECOUT (LUEO, ZXH, N, 'ZXH = (10**-5) d(ln N1/NT)/dZ :')
C
          call LINER  (1, LUEO)
          write (LUEO,102) IVLSW
  102     format(' ','Thus, IVLSW =',I3)
        end if
      end if
C     !END
      call BYE ('LOMBERS')
C
      return
      end
