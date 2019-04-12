      subroutine FANG
     $(N,NL,CHKI,JHACL,ICHSW,LU)
C
C     Rudolf Loeser, 1991 May 07
C---- Prints CHKI for FRANCIS.
C     !DASH
      save
C     !DASH
      real*8 CHKI
      integer ICHSW, J, JHACL, LU, N, NL
C     !DASH
      external ABJECT, PRIVET, LINER, HI, BYE
C
C               CHKI(N,NSL)
      dimension CHKI(N,*)
C
      call HI ('FANG')
C     !BEG
      if((JHACL.gt.0).and.(LU.gt.0)) then
C
        call ABJECT   (LU)
        if(ICHSW.gt.0) then
          write (LU,100)
  100     format(' ','CHKI, ionization rates due to collisions with ',
     $               'with Hydrogen atoms (included in final values ',
     $               'of CK)')
        else
          write (LU,101)
  101     format(' ','CHKI, ionization rates due to collisions with ',
     $               'with Hydrogen atoms (printed for reference ',
     $               'only)')
        end if
C
        do 103 J = 1,NL
          call LINER  (1, LU)
          write (LU,102) J
  102     format(' ','For level',I4)
          call PRIVET (LU, CHKI(1,J), N)
  103   continue
      end if
C
C     !END
      call BYE ('FANG')
C
      return
      end
