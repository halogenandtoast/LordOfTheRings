module LordOfTheRings.Location.Cards.AltarOfMidwinter (altarOfMidwinter) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDreadRealm.Helpers (reanimateTopOfDeck)

newtype AltarOfMidwinter = AltarOfMidwinter LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

altarOfMidwinter :: LocationCard AltarOfMidwinter
altarOfMidwinter = location AltarOfMidwinter Cards.altarOfMidwinter

-- | While Daechanar is in play, progress cannot be placed on Altar of Midwinter.
instance HasModifiersFor AltarOfMidwinter where
  getModifiersFor (AltarOfMidwinter a) = do
    daechanar <- selectAny (EnemyWithCardCode $ toCardCode Enemies.daechanar)
    when daechanar $ modifySelf a [ProgressLimit 0]

-- | Forced: At the end of the staging step, the first player must either reveal 1
-- additional card from the encounter deck, or reanimate the top card of his deck.
instance HasAbilities AltarOfMidwinter where
  getAbilities (AltarOfMidwinter a) = [mkAbility a 1 $ forced StagingStepEndsWindow]

instance RunMessage AltarOfMidwinter where
  runMessage msg l@(AltarOfMidwinter a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      deck <- (.deck) <$> getPlayer lead
      chooseOrRunOneM lead do
        questionLabeled "altarOfMidwinter.choose"
        causedBy a
        labeled "altarOfMidwinter.reveal" revealEncounterCard
        when (notNull deck) $ labeled "altarOfMidwinter.reanimate" $ reanimateTopOfDeck lead 1
      pure l
    _ -> AltarOfMidwinter <$> runMessage msg a
