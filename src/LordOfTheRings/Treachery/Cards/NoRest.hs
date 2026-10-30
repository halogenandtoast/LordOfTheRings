module LordOfTheRings.Treachery.Cards.NoRest (noRest) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (isSafe)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NoRest = NoRest TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

noRest :: TreacheryCard NoRest
noRest = treachery NoRest Cards.noRest

-- | Counts as a Condition attachment with the text: "Limit 1 per hero. Attached hero
-- cannot ready and does not collect resources during the resource phase. Discard No Rest
-- if the players travel to a safe location."
instance HasModifiersFor NoRest where
  getModifiersFor (NoRest a) = withHost a \host -> modified a host [CannotReady, CannotCollectResources]

-- | When Revealed: Attach to an exhausted hero, or to a ready hero if there are no
-- exhausted heroes. The first player chooses among them, each without a No Rest.
instance RunMessage NoRest where
  runMessage msg t@(NoRest attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      taken <- hostsOfTreachery (toCardCode Cards.noRest)
      let free = filter (\h -> not $ any (isTarget h) taken)
      exhausted <- free <$> select ExhaustedHero
      heroes <- if null exhausted then free <$> select AnyHero else pure exhausted
      chooseOrRunOneM lead do
        questionLabeled "noRest.attach"
        causedBy attrs
        targets heroes $ attachTreachery attrs.treacheryId
      pure t
    TravelTo lid -> do
      withHost attrs \_ -> whenM (isSafe lid) $ discard attrs
      NoRest <$> runMessage msg attrs
    _ -> NoRest <$> runMessage msg attrs
