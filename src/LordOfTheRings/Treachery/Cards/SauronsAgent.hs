module LordOfTheRings.Treachery.Cards.SauronsAgent (sauronsAgent) where

import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (getBellach, placeDamageOnActiveLocation)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SauronsAgent = SauronsAgent TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sauronsAgent :: TreacheryCard SauronsAgent
sauronsAgent = treachery SauronsAgent Cards.sauronsAgent

-- | When Revealed: Bellach makes an immediate attack against each player in turn order.
-- Then, deal 1 damage to the active location for each character destroyed by this effect.
-- Shadow: Deal 1 damage to the defending character.
--
-- ponytail: the characters destroyed are counted as those gone from play once the attacks
-- are over, the count before carried in the step; read the attacks' windows if another
-- effect discards a character meanwhile
instance RunMessage SauronsAgent where
  runMessage msg t@(SauronsAgent attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) ->
      getBellach >>= \case
        Nothing -> pure t
        Just bellach -> do
          inPlay <- length <$> selectCharacters AnyHero AnyAlly
          eachPlayer \p -> immediateAttack p bellach
          doStep inPlay msg
          pure t
    DoStep inPlay (WhenRevealed (isTarget attrs -> True)) -> do
      left <- length <$> selectCharacters AnyHero AnyAlly
      placeDamageOnActiveLocation attrs (max 0 $ inPlay - left)
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure t
    _ -> SauronsAgent <$> runMessage msg attrs
