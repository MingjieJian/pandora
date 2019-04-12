      subroutine NETTLE
     $(DUMP,XLM,N,NOPAC,KOPAC,CO,ISWA,T1,TR,CB,ISWE,S1,SR)
C
C     Rudolf Loeser, 1981 Jun 16
C---- Debug printout for absorbers and emitters.
C     !DASH
      save
C     !DASH
      real*8 CB, CO, S1, SR, T1, TR, XLM
      integer ISWA, ISWE, KOPAC, LUEO, N, NOPAC
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, EMEER, VECOUT, HI, BYE
C
C               CO(Nopac,N), ISWA(Nopac), T1(N), TR(N), KOPAC(Nopac),
      dimension CO(*),       ISWA(*),     T1(*), TR(*), KOPAC(*),
C
C               CB(Nopac,N), ISWE(Nopac), S1(N), SR(N)
     $          CB(*),       ISWE(*),     S1(*), SR(*)
C
      call HI ('NETTLE')
C     !BEG
      if(DUMP) then
        call MESHED   ('NETTLE', 2)
C
C----   Absorbers
        call EMEER  (XLM, ISWA, KOPAC, NOPAC, CO, N, LUEO, 'Absorbers')
        call VECOUT (LUEO, T1, N, 'T1')
        call VECOUT (LUEO, TR, N, 'TR')
C
C----   Emitters
        call EMEER  (XLM, ISWE, KOPAC, NOPAC, CB, N, LUEO, 'Emitters' )
        call VECOUT (LUEO, S1, N, 'S1')
        call VECOUT (LUEO, SR, N, 'SR')
C
        call MASHED   ('NETTLE')
      end if
C     !END
      call BYE ('NETTLE')
C
      return
      end
