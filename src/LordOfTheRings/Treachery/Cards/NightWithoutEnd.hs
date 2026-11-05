module LordOfTheRings.Treachery.Cards.NightWithoutEnd (nightWithoutEnd) where

import LordOfTheRings.Scenario.Scenarios.HelmsDeep.Helpers (defenseTokens, getDefenseOfHelmsDeep)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NightWithoutEnd = NightWithoutEnd TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Doomed 2, as its card def says.
nightWithoutEnd :: TreacheryCard NightWithoutEnd
nightWithoutEnd = treachery NightWithoutEnd Cards.nightWithoutEnd

{- | When Revealed: Either remove 1 resource token from The Defense of Helm's Deep, or
reveal 2 additional encounter cards. The token is offered only while there is one to
remove (FAQ 1.44: an option of "either" is taken only if it can be done in full).

Shadow: Return attacking enemy to the staging area after this attack.
-}
instance RunMessage NightWithoutEnd where
  runMessage msg t@(NightWithoutEnd attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      defense <- getDefenseOfHelmsDeep
      tokens <- defenseTokens
      chooseOrRunOneM you do
        questionLabeled "nightWithoutEnd.choose"
        causedBy attrs
        for_ defense \o -> when (tokens > 0) $ labeled "nightWithoutEnd.removeToken" $ removeTokens attrs o Resource 1
        labeled "nightWithoutEnd.reveal" $ revealEncounterCards 2
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> NightWithoutEnd <$> runMessage msg attrs
