module LordOfTheRings.Treachery.Cards.FightingAmongFriends (fightingAmongFriends) where

import LordOfTheRings.Card (cdSphere)
import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Sphere (Sphere (Baggins, Leadership, Lore, Spirit, Tactics))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FightingAmongFriends = FightingAmongFriends TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fightingAmongFriends :: TreacheryCard FightingAmongFriends
fightingAmongFriends = treachery FightingAmongFriends Cards.fightingAmongFriends

{- | When Revealed: The first player names a sphere. Then, all non-Baggins characters who do
not belong to the named sphere are removed from the quest. A character belongs to the
sphere printed on it; one of the four spheres of influence is named, as The Master's
Malice's.
Shadow: Attacking enemy gets +1 attack. (+2 attack instead if undefended.)
-}
instance RunMessage FightingAmongFriends where
  runMessage msg t@(FightingAmongFriends attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      committed <- selectCharacters CommittedHero CommittedAlly
      characters <- traverse (\c -> (c,) . cdSphere <$> characterCardDef c) committed
      lead <- getLead
      chooseOneM lead do
        questionLabeled "fightingAmongFriends.nameSphere"
        causedBy attrs
        for_ [(Leadership, "leadership"), (Lore, "lore"), (Spirit, "spirit"), (Tactics, "tactics")] \(sphere, key) ->
          labeled ("fightingAmongFriends." <> key)
            $ for_ [c | (c, s) <- characters, s /= Just Baggins, s /= Just sphere] removeFromQuest
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      attackModifier attrs eid (AttackModifier $ if null defenders then 2 else 1)
      pure t
    _ -> FightingAmongFriends <$> runMessage msg attrs
