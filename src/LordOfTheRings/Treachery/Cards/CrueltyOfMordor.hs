module LordOfTheRings.Treachery.Cards.CrueltyOfMordor (crueltyOfMordor) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CrueltyOfMordor = CrueltyOfMordor TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crueltyOfMordor :: TreacheryCard CrueltyOfMordor
crueltyOfMordor = treachery CrueltyOfMordor Cards.crueltyOfMordor

{- | When Revealed: Each player deals 1 damage to a character he controls. Then, each player
must choose: either deal 1 damage to each damaged character you control, or raise your
threat by 1 for each damaged character you control. The damaged characters are counted
once the first damage is dealt; a player with none has nothing to choose.
Shadow: Attacking enemy gets +2 Defense until the end of the phase.
-}
instance RunMessage CrueltyOfMordor where
  runMessage msg t@(CrueltyOfMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> dealDamageToOneCharacter attrs pid 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      eachPlayer \pid -> do
        damaged <- damagedCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
        unless (null damaged) $ chooseOneM pid do
          questionLabeled "crueltyOfMordor.choose"
          causedBy attrs
          labeled "crueltyOfMordor.damage" $ for_ damaged \c -> dealDamage attrs c 1
          labeledWith "crueltyOfMordor.threat" [intVar "n" (length damaged)] $ encounterRaisesThreat pid (length damaged)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifier (CardIdSource cid) eid (DefenseModifier 2)
      pure t
    _ -> CrueltyOfMordor <$> runMessage msg attrs
