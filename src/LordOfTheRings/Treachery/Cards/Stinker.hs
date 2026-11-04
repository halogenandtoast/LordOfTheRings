module LordOfTheRings.Treachery.Cards.Stinker (stinker) where

import LordOfTheRings.Card (EncounterCard (..), cdName, toCardCode)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Hero.Types (heroTokens)
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Stinker = Stinker TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

stinker :: TreacheryCard Stinker
stinker = treachery Stinker Cards.stinker

{- | Surge.
When Revealed: Gollum makes an immediate attack. If no attack was made this way, discard
all tokens from Sméagol and flip him to Gollum.
Gollum attacks the player he is engaged with; out of play, or barred from attacking, he
makes none. With neither side in play, as when his owner has been eliminated, nothing
happens.
Shadow: Raise your threat by 2 (by 4 instead if Gollum is engaged with you). Shuffle
Stinker back into the encounter deck.
-}
instance RunMessage Stinker where
  runMessage msg t@(Stinker attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      gollums <- select (EnemyWithCardCode $ toCardCode Enemies.gollumASitE)
      attacks <- forMaybeM gollums \eid -> do
        barred <- elem CannotAttack <$> getModifiers eid
        placement <- (.placement) <$> getEnemy eid
        pure $ case placement of
          EngagedWith pid | not barred -> Just (pid, eid)
          _ -> Nothing
      case attacks of
        (pid, eid) : _ -> immediateAttack pid eid
        [] -> do
          smeagols <- select (HeroWithTitle $ cdName Heroes.smeagol)
          for_ smeagols \hid -> do
            tokens <- heroTokens . toAttrs <$> getHero hid
            for_ (mapToList tokens) \(token, n) -> removeTokens attrs hid token n
            flipOver hid
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      gollum <- selectAny (EnemyWithCardCode (toCardCode Enemies.gollumASitE) <> EnemyEngagedWith (PlayerWithId pid))
      encounterRaisesThreat pid (if gollum then 4 else 2)
      shuffleShadowCardIntoEncounterDeck eid (MkEncounterCard cid (toCardCode attrs))
      pure t
    _ -> Stinker <$> runMessage msg attrs
