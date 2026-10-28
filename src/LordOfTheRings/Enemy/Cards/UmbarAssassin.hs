module LordOfTheRings.Enemy.Cards.UmbarAssassin (umbarAssassin) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (wasOptionallyEngaged)

newtype UmbarAssassin = UmbarAssassin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarAssassin :: EnemyCard UmbarAssassin
umbarAssassin = enemy UmbarAssassin Cards.umbarAssassin

-- | Archery 2.
-- Forced: When Umbar Assassin engages a player, that player must deal 3 damage to a hero
-- he controls. (Discard that hero instead if Umbar Assassin was not optionally engaged.)
instance HasAbilities UmbarAssassin where
  getAbilities (UmbarAssassin a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage UmbarAssassin where
  runMessage msg e@(UmbarAssassin attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        opted <- wasOptionallyEngaged attrs.id pid
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled $ if opted then "umbarAssassin.damage" else "umbarAssassin.discard"
          causedBy attrs
          targets heroes \h -> if opted then dealDamage attrs h 3 else discard h
      pure e
    _ -> UmbarAssassin <$> runMessage msg attrs
