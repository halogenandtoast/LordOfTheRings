module LordOfTheRings.Location.Cards.WickedHilltops (wickedHilltops) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Sack, Troll))

newtype WickedHilltops = WickedHilltops LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wickedHilltops :: LocationCard WickedHilltops
wickedHilltops = location WickedHilltops Cards.wickedHilltops

instance HasAbilities WickedHilltops where
  getAbilities (WickedHilltops a) = [mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)]

instance RunMessage WickedHilltops where
  runMessage msg l@(WickedHilltops attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> do
        allies <- select (AllyControlledBy (PlayerWithId p) <> NotAlly (AllyWithAttached Sack))
        chooseOrRunOneM p do
          questionLabeled "wickedHilltops.discard"
          causedBy attrs
          targets allies (discard attrs)
      pure l
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      troll <- selectAny (EnemyWithId eid <> EnemyWithTrait Troll)
      enemyGetsAttack attrs eid (if troll then 3 else 2)
      pure l
    _ -> WickedHilltops <$> runMessage msg attrs
