module LordOfTheRings.Treachery.Cards.RestByNight (restByNight) where

import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RestByNight = RestByNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

restByNight :: TreacheryCard RestByNight
restByNight = treachery RestByNight Cards.restByNight

-- | This effect cannot be canceled.
instance HasModifiersFor RestByNight where
  getModifiersFor (RestByNight a) = modifySelf a [WhenRevealedCannotBeCanceled]

{- | When Revealed: Either immediately end the quest phase and increase the pursuit value by
3, or deal 1 damage to each character committed to the quest. The first player chooses for
the players; the damage is offered only with a character committed, so it can be done in
full (FAQ 1.44).
-}
instance RunMessage RestByNight where
  runMessage msg t@(RestByNight attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      committed <- selectCharacters CommittedHero CommittedAlly
      chooseOrRunOneM lead do
        questionLabeled "restByNight.choose"
        causedBy attrs
        labeled "restByNight.endQuestPhase" $ doStep 1 msg
        when (notNull committed) $ labeled "restByNight.damage" $ for_ committed \c -> dealDamage attrs c 1
      pure t
    -- the quest phase ended as the choice resolves, not as it is offered
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      endQuestPhaseWithoutResolving
      increasePursuit 3
      pure t
    _ -> RestByNight <$> runMessage msg attrs
