module LordOfTheRings.Treachery.Cards.SuddenAssaultNightmare (suddenAssaultNightmare) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SuddenAssaultNightmare = SuddenAssaultNightmare TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

suddenAssaultNightmare :: TreacheryCard SuddenAssaultNightmare
suddenAssaultNightmare = treachery SuddenAssaultNightmare Cards.suddenAssaultNightmare

-- | When Revealed: Each player must either deal 2 damage to an Aflame location in play, or
-- reveal 1 card from the top of the encounter deck.
-- Shadow: Either deal 2 damage to an Aflame location in play or attacking enemy makes an
-- additional attack after this one.
instance RunMessage SuddenAssaultNightmare where
  runMessage msg t@(SuddenAssaultNightmare attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      burning <- map fst <$> getAflameLocations
      eachPlayer \p -> chooseOneM p do
        questionLabeled "suddenAssaultNightmare.choose"
        causedBy attrs
        targets burning \l -> dealDamage attrs l 2
        labeled "suddenAssaultNightmare.reveal" revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      burning <- map fst <$> getAflameLocations
      chooseOneM pid do
        questionLabeled "suddenAssaultNightmare.shadow"
        causedBy attrs
        targets burning \l -> dealDamage attrs l 2
        labeled "suddenAssaultNightmare.attack" $ additionalAttack pid eid
      pure t
    _ -> SuddenAssaultNightmare <$> runMessage msg attrs
