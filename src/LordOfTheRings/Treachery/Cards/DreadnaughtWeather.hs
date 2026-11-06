module LordOfTheRings.Treachery.Cards.DreadnaughtWeather (ballistaBarrage, overworked, violentTempest) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Trait (Trait (Ship))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DreadnaughtWeather = DreadnaughtWeather TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ballistaBarrage, overworked, violentTempest :: TreacheryCard DreadnaughtWeather
ballistaBarrage = treachery DreadnaughtWeather Cards.ballistaBarrage
overworked = treachery DreadnaughtWeather Cards.overworked
violentTempest = treachery DreadnaughtWeather Cards.violentTempest

alliesOf :: HasGame m => PlayerId -> m Int
alliesOf pid = length <$> select (AllyControlledBy $ PlayerWithId pid)

instance RunMessage DreadnaughtWeather where
  runMessage msg t@(DreadnaughtWeather attrs) = case msg of
    WhenRevealed (isTarget attrs -> True)
      | toCardCode attrs == toCardCode Cards.ballistaBarrage -> do
          roundModifier attrs GameTarget $ EachMatchingEnemy (CardWithTrait Ship) (GainKeyword $ Archery 2)
          pure t
      | toCardCode attrs == toCardCode Cards.overworked -> do
          roundModifier attrs GameTarget $ EachMatchingAlly (NotCard $ CardWithTrait Ship) Blank
          whenM (anyM (fmap (>= 7) . alliesOf) =<< getRemainingPlayers) revealEncounterCard
          pure t
      | otherwise -> do
          traverse_ (\e -> dealDamage attrs e 1) =<< select AnyEnemy
          doStep 1 msg
          pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      eachPlayer \pid -> do
        n <- length <$> exhaustedCharactersOf pid
        dealDamageDivided attrs pid n
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      if
        | toCardCode attrs == toCardCode Cards.ballistaBarrage -> do
            player <- getPlayer pid
            dealDamageDivided (CardIdSource cid) pid (if length player.discard >= 10 then 3 else 2)
        | toCardCode attrs == toCardCode Cards.overworked -> do
            n <- alliesOf pid
            enemyGetsAttack (CardIdSource cid) eid (if n >= 7 then 3 else 1)
        | otherwise -> for_ defenders \c -> dealDamage (CardIdSource cid) c 1
      pure t
    _ -> DreadnaughtWeather <$> runMessage msg attrs
