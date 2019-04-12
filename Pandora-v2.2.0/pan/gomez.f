      subroutine GOMEZ
     $(LU,N,TE,NHTSW,CEQHH,CEQMX,RSH,RNH1,H2N,HND,HNI,RABD,KRAB)
C
C     Rudolf Loeser, 1982 Jun 02
C---- Prints, for AMBER.
C     !DASH
      save
C     !DASH
      real*8 CEQHH, CEQMX, H2N, HND, HNI, RABD, RNH1, RSH, TE
      integer I, KRAB, LU, N, NHTSW
      character BLANK*1, LAB*45
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external ABJECT, LINER, HI, BYE
C
C               CEQHH(N), RSH(N), RNH1(N), H2N(N), HND(N), HNI(N,LIMP),
      dimension CEQHH(*), RSH(*), RNH1(*), H2N(*), HND(*), HNI(N,*),
C
C               RABD(N), TE(N)
     $          RABD(*), TE(*)
C
      call HI ('GOMEZ')
C     !BEG
      if(LU.gt.0) then
        call ABJECT (LU)
        write (LU,100)
  100   format(  ' ','Molecular Hydrogen.'//
     $           ' ','TE    = Electron temperature')
        if(NHTSW.eq.1) then
          write (LU,101) CEQMX
  101     format(' ','CEQHH = Temperature function from Kurucz, 1970, '
     $                        'SAO Sp. Rept. 309, pp. 55,267',2X,
     $                        'CEQMX =',1PE12.5)
        else if(NHTSW.eq.2) then
          write (LU,102)
  102     format(' ','CEQHH = K*TE/KH2(TE), where KH2(TE) is from ',
     $                        'Tsuji, 1973, Astron.Astrophys., ',
     $                        '23, 411.')
        else if(NHTSW.eq.3) then
          write (LU,103)
  103     format(' ','CEQHH = E(H2) from Table 2 of Kurucz, 1985, ',
     $                        'CfA Preprint # 2162.')
        end if
C     !EJECT
        write (LU,104)
  104   format(  ' ','H1    = Level 1 Hydrogen number density'/
     $           ' ','H2    = Molecular Hydrogen density, ',
     $                        '= CEQHH*(H1**2)'/
     $           ' ','HND   = Total Hydrogen number density ',
     $                        '(Atomic + Molecular)')
        LAB = BLANK
        if(KRAB.eq.1) then
          LAB = '             R1             RX           RABD'
          write (LU,105)
  105     format(' ','R1    = Ratio of Hydrogen in Level 1 to total'/
     $           ' ','RX    = Ratio of excited-state + ionized '
     $                        'Atomic Hydrogen to total'/
     $           ' ','RABD  = Atomic Hydrogen abundance ratio ',
     $                        'R1 + RX - (CHN + OHN)/HND,'/
     $           ' ','        where CHN and OHN are the CH and OH ',
     $                        'number densities, respectively')
        end if
        call LINER  (1, LU)
        write (LU,106) LAB
  106   format(  ' ',16X,'TE',10X,'CEQHH',13X,'H1',13X,'H2',12X,'HND',
     $               A45)
        call LINER  (1, LU)
        if(KRAB.eq.1) then
          write (LU,107) (I,TE(I),CEQHH(I),HNI(I,1),H2N(I),HND(I),
     $                    RNH1(I),RSH(I),RABD(I),I=1,N)
  107     format(5(' ',I3,1P8E15.5/))
        else
          write (LU,108) (I,TE(I),CEQHH(I),HNI(I,1),H2N(I),HND(I),
     $                    I=1,N)
  108     format(5(' ',I3,1P5E15.5/))
        end if
      end if
C     !END
      call BYE ('GOMEZ')
C
      return
      end
