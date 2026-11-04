module LordOfTheRings.Ally.Cards.GuardianOfEsgaroth (guardianOfEsgaroth) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (cdName, toCardDef)

newtype GuardianOfEsgaroth = GuardianOfEsgaroth AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

guardianOfEsgaroth :: AllyCard GuardianOfEsgaroth
guardianOfEsgaroth = ally GuardianOfEsgaroth Cards.guardianOfEsgaroth

-- | Guardian of Esgaroth gets +1 willpower, +1 attack, +1 defense and +1 hit point for
-- each different player card attachment it has. (Limit +3 to each.) Different by title.
instance HasModifiersFor GuardianOfEsgaroth where
  getModifiersFor (GuardianOfEsgaroth a) = do
    n <- min 3 . length . nub . map (cdName . toCardDef) <$> playerAttachmentsOn (toTarget a)
    when (n > 0) $ modifySelf a [WillpowerModifier n, AttackModifier n, DefenseModifier n, HitPointModifier n]
