module LordOfTheRings.Treachery.Cards.BitterReek (bitterReek) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BitterReek = BitterReek TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bitterReek :: TreacheryCard BitterReek
bitterReek = treachery BitterReek Cards.bitterReek

-- | Peril.
-- When Revealed: Either raise your threat by 1 for each character you control that is
-- committed to the quest, or deal 1 damage to each character committed to the quest.
-- Shadow: Raise your threat by the attacking enemy's threat.
instance RunMessage BitterReek where
  runMessage msg t@(BitterReek attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      yours <- selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId you)) (CommittedAlly <> AllyControlledBy (PlayerWithId you))
      committed <- selectCharacters CommittedHero CommittedAlly
      chooseOneM you do
        questionLabeled "bitterReek.choose"
        causedBy attrs
        labeled "bitterReek.raiseThreat" $ when (notNull yours) $ encounterRaisesThreat you (length yours)
        labeled "bitterReek.damage" $ for_ committed \c -> dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      threat <- getEnemyThreat eid
      when (threat > 0) $ encounterRaisesThreat pid threat
      pure t
    _ -> BitterReek <$> runMessage msg attrs
