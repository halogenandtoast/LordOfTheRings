module LordOfTheRings.Event.Cards.TheHousesOfHealing (theHousesOfHealing) where

import LordOfTheRings.Card (Card (PlayerCard), PlayerCard (..), cdCardType, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Trait (Trait (Healer))

newtype TheHousesOfHealing = TheHousesOfHealing EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, Entity)

theHousesOfHealing :: EventCard TheHousesOfHealing
theHousesOfHealing = eventWith TheHousesOfHealing Cards.theHousesOfHealing (criteriaL .~ DuringPhase RefreshPhase)

-- | You may exhaust any number of Healer characters you control as part of this card's
-- cost. Reduce the cost to play this card by 1 for each Healer character you exhaust in
-- this way.
instance HasModifiersFor TheHousesOfHealing where
  getModifiersFor (TheHousesOfHealing a) = modifySelf a [ReducedByExhausting Healer]

-- | Refresh Action: Choose a hero in any player's discard pile. Put that hero into play
-- under its owner's control, with 1 damage token on it.
instance RunMessage TheHousesOfHealing where
  runMessage msg e@(TheHousesOfHealing attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      players <- getRemainingPlayers
      discarded <- concat <$> for players \p -> do
        player <- getPlayer p
        pure [(p, c) | PlayerCard c <- player.discard, cdCardType (toCardDef c) == HeroType]
      chooseOrRunOneM pid do
        questionLabeled "theHousesOfHealing.hero"
        causedBy attrs
        showing [PlayerCard c | (_, c) <- discarded]
        for_ discarded \(owner, c) -> targeting c do
          removeFromDiscard owner c.pcId
          putHeroIntoPlay owner c
          placeTokens attrs c Damage 1
      pure e
    _ -> TheHousesOfHealing <$> runMessage msg attrs
