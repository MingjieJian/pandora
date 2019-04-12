      subroutine VESTRY
     $(N,Z,DEE,DOLD)
C
C     Rudolf Loeser, 2004 Feb 09
C---- Prints details of smoothing of d-coefficients.
C     !DASH
      save
C     !DASH
      real*8 DEE, DOLD, Z
      integer I, J, K, KODE, LODE, LUEO, N
      character RAT*16
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external YANEST, MESHED, MASHED, LINER, HI, BYE
C
C               Z(N), DEE(4,5,N), DOLD(4,5,N)
      dimension Z(*), DEE(4,5,*), DOLD(4,5,*)
C     !EJECT
C
      call HI ('VESTRY')
C     !BEG
      KODE = 0
      do 105 I = 1,4
        do 104 J = 1,5
          LODE = 0
          do 103 K = 1,N
            if(DEE(I,J,K).ne.DOLD(I,J,K)) then
              if(KODE.eq.0) then
                call MESHED ('VESTRY', 2)
                KODE = 1
                write (LUEO,100)
  100           format(' ','Details of smoothing of d-coefficients.')
              end if
              if(LODE.eq.0) then
                call LINER  (2, LUEO)
                LODE = 1
                write(LUEO,101) I,J
  101           format(' ','For d(',I1,',',I1,')'//
     $                 ' ',4X,'i',15X,'Z',17X,'old',9X,'new/old',
     $                     17X,'new')
                call LINER  (1, LUEO)
              end if
              call YANEST   (DEE(I,J,K), DOLD(I,J,K), RAT)
              write (LUEO,102) K,Z(K),DOLD(I,J,K),RAT,DEE(I,J,K)
  102         format(' ',I5,1PE16.8,E20.8,A16,E20.8)
            end if
  103     continue
  104   continue
  105 continue
      if(KODE.eq.1) then
        call MASHED         ('VESTRY')
      end if
C     !END
      call BYE ('VESTRY')
C
      return
      end
