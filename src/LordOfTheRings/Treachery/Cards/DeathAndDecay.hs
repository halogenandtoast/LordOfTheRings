module LordOfTheRings.Treachery.Cards.DeathAndDecay (deathAndDecay) where

import LordOfTheRings.Card (cdName, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesAttachments)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DeathAndDecay = DeathAndDecay TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deathAndDecay :: TreacheryCard DeathAndDecay
deathAndDecay = treachery DeathAndDecay Cards.deathAndDecay

-- | When Revealed: Discard the top 3 cards of each player's deck. Each player discards each
-- attachment he controls that has the same title as a card in his discard pile. If no
-- attachments are discarded from play by this effect, Death and Decay gains surge.
instance RunMessage DeathAndDecay where
  runMessage msg t@(DeathAndDecay attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> discardTopOfDeck p 3
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- traverse getPlayer =<< getRemainingPlayers
      attachments <- getsGame (toList . entitiesAttachments . gameEntities)
      let doomed =
            [ toTarget x
            | p <- players
            , x <- attachments
            , (toAttrs x).controller == p.id
            , cdName (toCardDef x) `elem` map (cdName . toCardDef) p.discard
            ]
      for_ doomed discard
      when (null doomed) revealEncounterCard
      pure t
    _ -> DeathAndDecay <$> runMessage msg attrs
