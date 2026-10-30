module LordOfTheRings.Treachery.Cards.DeathAndCalamity (deathAndCalamity) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheDreadRealm.Helpers (reanimateTopOfDeck, sorceryCardsInPlay)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DeathAndCalamity = DeathAndCalamity TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deathAndCalamity :: TreacheryCard DeathAndCalamity
deathAndCalamity = treachery DeathAndCalamity Cards.deathAndCalamity

{- | When Revealed: Attach Death and Calamity to the current quest. Then, the first player
reanimates the top X cards of his deck, where X is the number of copies of Death and
Calamity in play: the others attached, and this one once it is.
Shadow: Attacking enemy gets +X attack, where X is the number of Sorcery cards in play.
-}
instance RunMessage DeathAndCalamity where
  runMessage msg t@(DeathAndCalamity attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      quest <- getCurrentQuest
      traverse_ (attachTreachery attrs.treacheryId) quest
      others <- getsGame \g ->
        length [() | c <- toList g.entities.treacheries, toCardCode c == toCardCode attrs, c.id /= attrs.id, AttachedTo _ <- [c.placement]]
      lead <- getLead
      reanimateTopOfDeck lead (others + length quest)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      n <- length <$> sorceryCardsInPlay
      when (n > 0) $ enemyGetsAttack attrs eid n
      pure t
    _ -> DeathAndCalamity <$> runMessage msg attrs
