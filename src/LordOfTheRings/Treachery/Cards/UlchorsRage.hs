module LordOfTheRings.Treachery.Cards.UlchorsRage (ulchorsRage) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UlchorsRage = UlchorsRage TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ulchorsRage :: TreacheryCard UlchorsRage
ulchorsRage = treachery UlchorsRage Cards.ulchorsRage

-- | When Revealed: Thane Ulchor makes an immediate attack against the first player. If no
-- attack is made this way, Ulchor's Rage gains surge: as when he is not in play, or cannot
-- attack the first player.
-- Shadow: Deal 1 damage to the defending character.
instance RunMessage UlchorsRage where
  runMessage msg t@(UlchorsRage attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      thanes <- filterM (canAttack lead) =<< select thaneUlchor
      case thanes of
        thane : _ -> immediateAttack lead thane
        [] -> revealEncounterCard
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure t
    _ -> UlchorsRage <$> runMessage msg attrs
   where
    canAttack lead e = do
      mods <- getModifiers (EnemyTarget e)
      pure $ CannotAttack `notElem` mods && CannotAttackPlayer lead `notElem` mods
